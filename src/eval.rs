use std::{slice, str};

use thiserror::Error;

use crate::{
    asset::{Asset, BlockId},
    code::{CaptureSource, Instr, InstrIndex, Op2},
    sched::NotifyToken,
    space::{OutOfSpace, Space, SpaceAddr},
    task::Task,
    typing::{RecordLayout, TypeId},
    worker::WorkerContext,
};

#[derive(Debug, Default)]
pub struct Eval {
    frames: Vec<Frame>,
}

#[derive(Debug)]
struct Frame {
    closure: Closure,
    instr_pointer: InstrIndex,
    call_dst: Option<usize>,
    values: Vec<Value>,
}

impl Eval {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn init(&mut self, closure: Closure, asset: &Asset) -> Result<(), ArityError> {
        self.push_frame(closure, &[], asset)
    }
}

// optimization plan: pack Value into a u64 word. 24 bits for `type_id` and 40
// bits for `data`. assuming Space alignment >= 8 (or else a 4 byte data can be
// stored inline), 40 bits address space = 2^(40+3) byte or 8 TB heap space

const _: () = assert!(size_of::<SpaceAddr>() == size_of::<usize>());
const _: () = assert!(align_of::<SpaceAddr>() == align_of::<usize>());
#[derive(Debug, Clone, Copy)]
pub struct Value {
    type_id: TypeId,
    data: u64, // embedded data for inline types, SpaceAddr pointing to actual data for others
}

// helpers to maintain compatibility after the optimization is applied
impl Value {
    fn new(type_id: TypeId, addr: SpaceAddr) -> Self {
        Self {
            type_id,
            data: addr as _,
        }
    }

    fn new_inline(type_id: TypeId, data: u64) -> Self {
        Self { type_id, data }
    }

    fn new_atom(type_id: TypeId) -> Self {
        Self::new_inline(type_id, u64::MAX)
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }

    fn data(&self) -> u64 {
        self.data
    }

    fn addr(&self) -> SpaceAddr {
        self.data as _
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::new_atom(TypeId::VACANT)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Closure {
    pub block_id: BlockId,
    // addresses of cells. reduced from `Value`s with `TypeId::CELL` because captured
    // values are always cells (assuming valid instruction sequence produced with
    // `compile` module), so save the memory and type check
    captured: [SpaceAddr; 16],
    num_captured: usize,
}

impl Closure {
    pub fn new(block_id: BlockId) -> Self {
        Self {
            block_id,
            captured: [SpaceAddr::MAX; 16],
            num_captured: 0,
        }
    }

    fn capture(&mut self, addr: SpaceAddr) {
        assert!(
            self.num_captured < 16,
            "capturing more than 16 values is not supported"
        );
        self.captured[self.num_captured] = addr;
        self.num_captured += 1
    }
}

#[derive(Error, Debug)]
#[error("arity error: accept {expected} arguments, but got {actual}")]
pub struct ArityError {
    pub expected: usize,
    pub actual: usize,
}

impl Eval {
    fn push_frame(
        &mut self,
        closure: Closure,
        args: &[Value],
        asset: &Asset,
    ) -> Result<(), ArityError> {
        let block = asset.get_block(closure.block_id);
        if args.len() != block.num_param {
            return Err(ArityError {
                expected: block.num_param,
                actual: args.len(),
            });
        }
        let mut values = vec![Value::default(); block.num_value];
        assert!(values.len() >= args.len());
        for (dst, src) in values.iter_mut().zip(args) {
            *dst = *src
        }
        self.frames.push(Frame {
            closure,
            instr_pointer: 0,
            call_dst: None,
            values,
        });
        Ok(())
    }
}

struct Unit;

struct String {
    buf: SpaceAddr,
    len: usize,
}

#[derive(Debug, Clone, Copy)]
struct Signal(NotifyToken);

pub enum ExecuteStatus {
    Waiting(NotifyToken),
    Exited,
}

#[derive(Error, Debug)]
pub enum ExecuteError {
    #[error("{0}")]
    Space(#[from] OutOfSpace),
    #[error("{0}")]
    Type(#[from] TypeError),
    #[error("{0}")]
    Arity(#[from] ArityError),
    #[error("{0}")]
    Intrinsic(#[from] intrinsics::Error),
    #[error("perform record operation on other types")]
    NotRecord,
    #[error("unexpected attribute {0}")]
    UnexpectedAttr(std::string::String),
    #[error("missing attributes during record initialization")]
    MissingAttr,
}

impl Eval {
    pub fn execute(
        &mut self,
        context: &mut WorkerContext,
        asset: &Asset,
    ) -> Result<ExecuteStatus, ExecuteError> {
        // the contract here is `execute` should only be called after calling `init`,
        // and should not be called again after returning Exited
        let mut frame = self.frames.last_mut().expect("last frame exists");
        'control: loop {
            let block = asset.get_block(frame.closure.block_id);
            // optimize for sequentially execute instructions
            for instr in &block.instrs[frame.instr_pointer..] {
                // invariant: instr = block.instrs[frame.instr_pointer]
                // at the end of this loop, instr_pointer is incremented to maintain this
                // invariant. the increment is at the end for re-executing failed instructions
                // in recoverable cases like OutOfSpace. also, every adjustment of instr_pointer
                // (for control flow) is followed by a `break` or `continue` of the outer
                // `'control`
                tracing::trace!(%frame.instr_pointer, %block.name, ?instr, "execute");
                match instr {
                    Instr::Jump(instr_index, cond) => {
                        if match cond {
                            None => true,
                            Some(cond) => {
                                let target = frame.values[cond.pattern].load_type_id()?;
                                frame.values[cond.scrutinee].type_id() == target
                            }
                        } {
                            frame.instr_pointer = *instr_index;
                            continue 'control;
                        }
                    }

                    Instr::Spawn(closure) => {
                        let mut eval = Self::new();
                        eval.init(frame.values[*closure].load_closure(&context.space)?, asset)?;
                        context.sched.spawn(Task::new(eval));
                    }
                    Instr::Call(dst, closure, args) => {
                        let closure_value = frame.values[*closure];
                        let arg_values = args
                            .iter()
                            .map(|arg| frame.values[*arg])
                            .collect::<Vec<_>>();
                        let replaced = frame.call_dst.replace(*dst);
                        assert!(replaced.is_none());
                        // borrow to `self` via `frame` ends here
                        self.push_frame(
                            closure_value.load_closure(&context.space)?,
                            &arg_values,
                            asset,
                        )?;
                        // reborrow
                        frame = self.frames.last_mut().expect("last frame exists");
                        continue 'control;
                    }
                    Instr::Return(index) => {
                        let return_value = frame.values[*index];
                        // borrow to `self` via `frame` ends here
                        self.frames.pop();
                        let Some(last_frame) = self.frames.last_mut() else {
                            return_value.load_unit()?; // should we make a dedicated error variant?
                            break 'control Ok(ExecuteStatus::Exited);
                        };
                        // reborrow
                        frame = last_frame;
                        let dst = frame.call_dst.take().expect("call destination must be set");
                        frame.values[dst] = return_value;
                        frame.instr_pointer += 1; // current Call instruction is done
                        continue 'control;
                    }

                    Instr::MakeSignal(dst) => {
                        // leaking a NotifyToken should be fine
                        let signal = Signal(context.sched.alloc_notify_token());
                        frame.values[*dst] = Value::alloc_signal(signal, &mut context.space)?
                    }
                    Instr::Wait(signal) => {
                        let Signal(notify_token) =
                            frame.values[*signal].load_signal(&context.space)?;
                        // on the next `execute`, start from the next instruction
                        frame.instr_pointer += 1;
                        break 'control Ok(ExecuteStatus::Waiting(notify_token));
                    }
                    Instr::Notify(signal) => {
                        let Signal(notify_token) =
                            frame.values[*signal].load_signal(&context.space)?;
                        context.sched.notify_clear(notify_token)
                    }

                    Instr::Intrinsic(intrinsic, indexes) => {
                        intrinsic(&mut frame.values, indexes, context)?
                    }
                    Instr::Copy(dst, src) => {
                        let src_value = frame.values[*src];
                        frame.values[*dst] = src_value
                    }

                    Instr::MakeClosure(dst, block_id) => {
                        let closure_value =
                            Value::alloc_closure(Closure::new(*block_id), &mut context.space)?;
                        frame.values[*dst] = closure_value
                    }
                    Instr::Promote(index) => {
                        frame.values[*index] =
                            Value::alloc_cell(frame.values[*index], &mut context.space)?
                    }
                    Instr::Capture(dst, source) => {
                        let addr = match source {
                            CaptureSource::Original(index) => {
                                let captured_value = frame.values[*index];
                                captured_value.ensure_type(TypeId::CELL).unwrap();
                                captured_value.addr()
                            }
                            CaptureSource::Transitive(index) => frame.closure.captured[*index],
                        };
                        frame.values[*dst]
                            .get_closure_mut(&mut context.space)
                            .unwrap()
                            .capture(addr)
                    }
                    // the 0 position corresponds to Ref layout
                    Instr::GetCaptured(dst, captured_index) => {
                        frame.values[*dst] = unsafe {
                            value_slice_load(
                                &context.space,
                                frame.closure.captured[*captured_index],
                                0,
                            )
                        }
                    }
                    Instr::SetCaptured(captured_index, index) => unsafe {
                        value_slice_store(
                            &mut context.space,
                            frame.closure.captured[*captured_index],
                            0,
                            frame.values[*index],
                        )
                    },
                    Instr::Demote(index) => {
                        let value = frame.values[*index];
                        value.ensure_type(TypeId::CELL).unwrap();
                        frame.values[*index] =
                            unsafe { value_slice_load(&context.space, value.addr(), 0) }
                    }
                    Instr::SetPromoted(dst, src) => {
                        let dst_value = frame.values[*dst];
                        dst_value.ensure_type(TypeId::CELL).unwrap();
                        unsafe {
                            value_slice_store(
                                &mut context.space,
                                dst_value.addr(),
                                0,
                                frame.values[*src],
                            )
                        }
                    }

                    Instr::MakeRecordType(dst, layout) => {
                        let type_id = context.registry.add_record_type(layout.clone());
                        frame.values[*dst] = Value::new_type_id(type_id)
                    }
                    Instr::MakeRecord(dst, type_id, attrs) => {
                        let type_id = frame.values[*type_id].load_type_id()?;
                        let Some(RecordLayout(layout)) =
                            context.registry.get_record_layout(type_id)
                        else {
                            return Err(ExecuteError::NotRecord);
                        };
                        let mut record_attrs = vec![Value::default(); layout.len()];
                        for (attr, index) in attrs {
                            let Some(pos) =
                                layout.iter().position(|layout_attr| layout_attr == attr)
                            else {
                                return Err(ExecuteError::UnexpectedAttr(
                                    asset.get_string(*attr).into(),
                                ));
                            };
                            record_attrs[pos] = frame.values[*index]
                        }
                        if attrs.len() != layout.len() {
                            return Err(ExecuteError::MissingAttr);
                        }
                        frame.values[*dst] =
                            Value::alloc_record(type_id, record_attrs, &mut context.space)?
                    }
                    Instr::GetAttr(dst, record, attr) => {
                        let record_value = &frame.values[*record];
                        let Some(RecordLayout(layout)) =
                            context.registry.get_record_layout(record_value.type_id())
                        else {
                            return Err(ExecuteError::NotRecord);
                        };
                        let Some(pos) = layout.iter().position(|layout_attr| layout_attr == attr)
                        else {
                            return Err(ExecuteError::UnexpectedAttr(
                                asset.get_string(*attr).into(),
                            ));
                        };
                        frame.values[*dst] =
                            unsafe { value_slice_load(&context.space, record_value.addr(), pos) }
                    }
                    Instr::SetAttr(record, attr, index) => {
                        let record_value = &frame.values[*record];
                        let Some(RecordLayout(layout)) =
                            context.registry.get_record_layout(record_value.type_id())
                        else {
                            return Err(ExecuteError::NotRecord);
                        };
                        let Some(pos) = layout.iter().position(|layout_attr| layout_attr == attr)
                        else {
                            return Err(ExecuteError::UnexpectedAttr(
                                asset.get_string(*attr).into(),
                            ));
                        };
                        unsafe {
                            value_slice_store(
                                &mut context.space,
                                record_value.addr(),
                                pos,
                                frame.values[*index],
                            )
                        }
                    }

                    Instr::MakeUnit(dst) => frame.values[*dst] = Value::new_unit(),
                    Instr::MakeString(dst, literal) => {
                        frame.values[*dst] = Value::alloc_string(literal, &mut context.space)?
                    }
                    Instr::MakeI32(dst, value) => {
                        frame.values[*dst] = Value::new_int32(*value);
                    }

                    Instr::Op2(dst, op, a, b) => {
                        let dst_value = match op {
                            Op2::Add
                            | Op2::Sub
                            | Op2::Eq
                            | Op2::Ne
                            | Op2::Mul
                            | Op2::Div
                            | Op2::Rem
                            | Op2::Lt
                            | Op2::Gt
                            | Op2::Le
                            | Op2::Ge => {
                                let a = frame.values[*a].load_int32()?;
                                let b = frame.values[*b].load_int32()?;
                                match op {
                                    Op2::Add => Value::new_int32(a + b),
                                    Op2::Sub => Value::new_int32(a - b),
                                    Op2::Eq => Value::new_bool(a == b),
                                    Op2::Ne => Value::new_bool(a != b),
                                    Op2::Mul => Value::new_int32(a * b),
                                    Op2::Div => Value::new_int32(a / b),
                                    Op2::Rem => Value::new_int32(a % b),
                                    Op2::Lt => Value::new_bool(a < b),
                                    Op2::Gt => Value::new_bool(a > b),
                                    Op2::Le => Value::new_bool(a <= b),
                                    Op2::Ge => Value::new_bool(a >= b),
                                    // _ => unreachable!(),
                                }
                            }
                        };
                        frame.values[*dst] = dst_value
                    }
                }

                frame.instr_pointer += 1;
            }
            unreachable!("block should terminate with Return")
        }
    }
}

unsafe fn value_slice_load(space: &Space, addr: SpaceAddr, pos: usize) -> Value {
    *unsafe { space.typed_get(addr + pos * size_of::<Value>()) }
}

unsafe fn value_slice_store(space: &mut Space, addr: SpaceAddr, pos: usize, value: Value) {
    unsafe { space.typed_write(addr + pos * size_of::<Value>(), value) }
}

#[derive(Error, Debug)]
#[error("type error: expected {expected:?}, actual {actual:?}")]
pub struct TypeError {
    pub expected: TypeId,
    pub actual: TypeId,
}

impl Value {
    fn ensure_type(&self, expected: TypeId) -> Result<(), TypeError> {
        if self.type_id() != expected {
            return Err(TypeError {
                expected,
                actual: self.type_id(),
            });
        }
        Ok(())
    }

    // helper methods for essential types
    // new_x        (X) -> Self                     construct inline type
    // alloc_x      (X, &mut Space) -> Self?        construct heap-allocated type
    // load_x       (&self) -> X?                   access inline type
    // store_x      (&self, X) -> ()?
    // load_x       (&self, &Space) -> X?           access heap-allocated type with
    // store_x      (&self, X, &mut Space) -> ()?     value semantic
    // get_x        (&self, &Space) -> &X?          ... with reference-semantic
    // get_x_mut    (&self, &mut Space) -> &mut X?
    // every type has either one of `new_x` or `alloc_x`. if both `alloc_x` and
    // `store_x` are provided, then `alloc_x` is equivalent to wrapping the result
    // of `space.typed_alloc::<X>()` in a X-typed Value and then `store_x`, but
    // `alloc_x` is more convenient and saves one unnecessary type check
    // for read-only types (e.g., Signal), only `load_x` and/or `get_x` may be
    // provided. for heap-allocated mutable types, which of the four access methods
    // are provided is based on an on-demand manner and depends on the access
    // pattern. For example, Closure provides `get_closure_mut` for implementing
    // Capture and `load_closure` for implementing Call, String provides
    // `get_string` and `get_string_mut` for implementing intrinsics, etc.

    // TypeId
    fn new_type_id(TypeId(id): TypeId) -> Self {
        Self::new_inline(TypeId::TYPE_ID, id as _)
    }

    fn load_type_id(&self) -> Result<TypeId, TypeError> {
        self.ensure_type(TypeId::TYPE_ID)?;
        Ok(TypeId(self.data() as _))
    }

    // Closure
    fn alloc_closure(closure: Closure, space: &mut Space) -> Result<Self, OutOfSpace> {
        let addr = space.typed_alloc::<Closure>()?;
        unsafe { space.typed_write(addr, closure) }
        Ok(Self::new(TypeId::CLOSURE, addr))
    }

    fn load_closure(&self, space: &Space) -> Result<Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(*unsafe { space.typed_get(self.addr()) })
    }

    fn get_closure_mut<'a>(&self, space: &'a mut Space) -> Result<&'a mut Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(unsafe { space.typed_get_mut(self.addr()) })
    }

    // cell type
    fn alloc_cell(value: Value, space: &mut Space) -> Result<Self, OutOfSpace> {
        let addr = space.typed_alloc::<Value>()?;
        unsafe { space.typed_write(addr, value) }
        Ok(Self::new(TypeId::CELL, addr))
    }
    // access to cell is performed directly with SpaceAddr, not going through Value

    // Signal
    fn alloc_signal(signal: Signal, space: &mut Space) -> Result<Self, OutOfSpace> {
        let addr = space.typed_alloc::<Signal>()?;
        unsafe { space.typed_write(addr, signal) };
        Ok(Self::new(TypeId::SIGNAL, addr))
    }

    fn load_signal(&self, space: &Space) -> Result<Signal, TypeError> {
        self.ensure_type(TypeId::SIGNAL)?;
        Ok(*unsafe { space.typed_get(self.addr()) })
    }

    // unit type
    fn new_unit() -> Self {
        Self::new_atom(TypeId::UNIT)
    }

    fn load_unit(&self) -> Result<Unit, TypeError> {
        self.ensure_type(TypeId::UNIT)?;
        Ok(Unit)
    }

    // String
    fn alloc_string(literal: &str, space: &mut Space) -> Result<Self, OutOfSpace> {
        let len = literal.len();
        // allocate buffer first, otherwise if buffer fails to allocate while String
        // itself succeeds, marking String would encounter invalid SpaceAddr
        let buf = space.alloc(len, 1)?;
        space.get_mut(buf, len).copy_from_slice(literal.as_bytes());
        let addr = space.typed_alloc::<String>()?;
        unsafe { space.typed_write(addr, String { buf, len }) }
        Ok(Self::new(TypeId::STRING, addr))
    }

    fn get_str<'a>(&self, space: &'a Space) -> Result<&'a str, TypeError> {
        self.ensure_type(TypeId::STRING)?;
        let string = unsafe { space.typed_get::<String>(self.addr()) };
        Ok(unsafe { str::from_utf8_unchecked(space.get(string.buf, string.len)) })
    }

    // Int32 type
    fn new_int32(value: i32) -> Self {
        let mut data = [0; size_of::<u64>()];
        data[..size_of::<i32>()].copy_from_slice(&value.to_ne_bytes());
        Self::new_inline(TypeId::INT32, u64::from_ne_bytes(data))
    }

    fn load_int32(&self) -> Result<i32, TypeError> {
        self.ensure_type(TypeId::INT32)?;
        let mut value = [0; size_of::<i32>()];
        value.copy_from_slice(&self.data().to_ne_bytes()[..size_of::<i32>()]);
        Ok(i32::from_ne_bytes(value))
    }

    // boolean types
    pub fn new_true() -> Self {
        Self::new_atom(TypeId::TRUE)
    }

    pub fn new_false() -> Self {
        Self::new_atom(TypeId::FALSE)
    }

    pub fn new_bool(value: bool) -> Self {
        if value {
            Self::new_true()
        } else {
            Self::new_false()
        }
    }

    // record types
    fn alloc_record(
        type_id: TypeId,
        attrs: Vec<Value>,
        space: &mut Space,
    ) -> Result<Self, OutOfSpace> {
        let addr = space.alloc(size_of::<Value>() * attrs.len(), align_of::<Value>())?;
        let buf = space
            .get_mut(addr, size_of::<Value>() * attrs.len())
            .as_mut_ptr()
            .cast::<Value>();
        let record_attrs = unsafe { slice::from_raw_parts_mut(buf, attrs.len()) };
        for (dst, src) in record_attrs.iter_mut().zip(attrs) {
            *dst = src
        }
        Ok(Self::new(type_id, addr))
    }
}

pub mod intrinsics {
    use std::ptr::{copy, copy_nonoverlapping};

    use thiserror::Error;

    use crate::{
        asset::Asset,
        code::{ValueIndex, instr::Intrinsic},
        eval::{ExecuteError, Signal, TypeError, Value, value_slice_load, value_slice_store},
        space::{OutOfSpace, Space, SpaceAddr},
        typing::{RecordLayout, TypeId, TypeRegistry},
        worker::WorkerContext,
    };

    pub fn preload(asset: &mut Asset) {
        START.with(|_| {}); // initialize the thread-local variable
        asset.intrinsics = [
            ("list_cap", list_cap as Intrinsic),
            ("list_copy_within", list_copy_within),
            ("list_copy", list_copy),
            ("list_len", list_len),
            ("list_load", list_load),
            ("list_new", list_new),
            ("list_set_len", list_set_len),
            ("list_store", list_store),
            ("oracle_advance_signal", oracle_advance_signal),
            ("time_since_start", time_since_start),
            ("trace", trace),
            ("type_object", type_object),
        ]
        .map(|(s, f)| (s.into(), f))
        .into()
    }

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("{0}")]
        ParseDuration(#[from] humantime::DurationError),
    }

    // impl<E: Into<Error>> From<E> for ExecuteError {
    //     fn from(err: E) -> Self {
    //         ExecuteError::from(err.into())
    //     }
    // }

    fn trace(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let value = values[indexes[0]];

        fn format_value(value: Value, space: &Space, registry: &TypeRegistry) -> String {
            if let Ok(message) = value.get_str(space) {
                message.into()
            } else if let Ok(int32) = value.load_int32() {
                int32.to_string()
            } else if let Some(RecordLayout(attrs)) = registry.get_record_layout(value.type_id()) {
                let attrs = attrs
                    .iter()
                    .enumerate()
                    .map(|(i, &attr)| {
                        let attr_value = unsafe { value_slice_load(space, value.addr(), i) };
                        // TODO get interned string
                        format!("{} = {}", attr, format_value(attr_value, space, registry))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("record({:?})[{attrs}]", value.type_id())
            } else {
                "<unknown>".into() // TODO
            }
        }

        tracing::info!(value = format_value(value, &context.space, &context.registry));
        Ok(())
    }

    fn oracle_advance_signal(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        values[indexes[0]] =
            Value::alloc_signal(Signal(context.oracle_advance), &mut context.space)?;
        Ok(())
    }

    fn type_object(
        values: &mut [Value],
        indexes: &[ValueIndex],
        _: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let id = values[indexes[1]].load_int32()?;
        values[indexes[0]] = Value::new_type_id(TypeId(id as _));
        Ok(())
    }

    thread_local!(static START: std::time::Instant = std::time::Instant::now());
    fn time_since_start(
        values: &mut [Value],
        indexes: &[ValueIndex],
        _: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let duration = START.with(|start| start.elapsed());
        assert!(
            duration.as_secs() < i32::MAX as _,
            "program has been running for too long"
        );
        values[indexes[0]] = Value::new_int32(duration.as_secs() as _);
        values[indexes[1]] = Value::new_int32(duration.subsec_nanos() as _);
        Ok(())
    }

    #[derive(Debug, Clone, Copy)]
    struct List {
        buf: SpaceAddr,
        // these are kept natively (instead of as record attributes) for garbage
        // collection purposes. `len` is for recursing into referenced values, `cap` is
        // reset to `len` after the collection, which only allocate and copy up to `len`
        len: usize,
        cap: usize,
    }

    impl Value {
        fn alloc_list(cap: usize, space: &mut Space) -> Result<Self, OutOfSpace> {
            let buf = space.alloc(size_of::<Value>() * cap, align_of::<Value>())?;
            let addr = space.typed_alloc::<List>()?;
            unsafe { space.typed_write(addr, List { buf, len: 0, cap }) }
            Ok(Self::new(TypeId::LIST, addr))
        }

        // it is also ok to have a load_list
        fn get_list<'a>(&self, space: &'a Space) -> Result<&'a List, TypeError> {
            self.ensure_type(TypeId::LIST)?;
            Ok(unsafe { space.typed_get::<List>(self.addr()) })
        }

        fn get_list_mut<'a>(&self, space: &'a mut Space) -> Result<&'a mut List, TypeError> {
            self.ensure_type(TypeId::LIST)?;
            Ok(unsafe { space.typed_get_mut::<List>(self.addr()) })
        }
    }

    fn list_new(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let cap = values[indexes[1]].load_int32()?;
        if cap < 0 {
            todo!()
        }
        values[indexes[0]] = Value::alloc_list(cap as _, &mut context.space)?;
        Ok(())
    }

    // list_push is implemented from lang side, with the following intrinsics
    fn list_len(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let list = values[indexes[1]].get_list(&context.space)?;
        values[indexes[0]] = Value::new_int32(list.len as _);
        Ok(())
    }

    fn list_set_len(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let list = values[indexes[0]].get_list_mut(&mut context.space)?;
        let len = values[indexes[1]].load_int32()?;
        assert!(len >= 0);
        assert!(len as usize <= list.cap);
        list.len = len as _;
        Ok(())
    }

    fn list_cap(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let list = values[indexes[1]].get_list(&context.space)?;
        values[indexes[0]] = Value::new_int32(list.cap as _);
        Ok(())
    }

    // while this can be achieved by per-value load + store, expect a performance
    // gap to necessitate
    fn list_copy(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let src = values[indexes[0]].get_list(&context.space)?;
        let dst = values[indexes[1]].get_list(&context.space)?;
        let len = src.len;
        assert!(dst.cap >= len);
        let src_buf = src.buf;
        let dst_buf = dst.buf;
        assert_ne!(src_buf, dst_buf);
        unsafe {
            copy_nonoverlapping(
                context.space.typed_get::<Value>(src_buf),
                context.space.typed_get_mut(dst_buf),
                len,
            )
        }
        Ok(())
    }

    fn list_load(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let list = values[indexes[1]].get_list(&context.space)?;
        let pos = values[indexes[2]].load_int32()?;
        assert!(pos >= 0);
        assert!((pos as usize) < list.len); // avoid exposing garbage value to lang
        values[indexes[0]] = unsafe { value_slice_load(&context.space, list.buf, pos as _) };
        Ok(())
    }

    fn list_store(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let list = values[indexes[0]].get_list(&context.space)?;
        let pos = values[indexes[1]].load_int32()?;
        assert!(pos >= 0);
        assert!((pos as usize) < list.cap); // as long as not overflowing it is fine
        let buf = list.buf;
        unsafe { value_slice_store(&mut context.space, buf, pos as _, values[indexes[2]]) }
        Ok(())
    }

    // similar to list_copy but for a performant list_insert and list_remove
    fn list_copy_within(
        values: &mut [Value],
        indexes: &[ValueIndex],
        context: &mut WorkerContext,
    ) -> Result<(), ExecuteError> {
        let list = values[indexes[0]].get_list(&context.space)?;
        let src = values[indexes[1]].load_int32()?;
        let len = values[indexes[2]].load_int32()?;
        let dst = values[indexes[3]].load_int32()?;
        assert!(src >= 0);
        assert!(dst >= 0);
        assert!(len > 0);
        let src = src as usize;
        let dst = dst as usize;
        let len = len as usize;
        assert!(src + len <= list.len);
        assert!(dst + len <= list.len);
        unsafe {
            copy(
                context
                    .space
                    .typed_get::<Value>(list.buf + src * size_of::<Value>()),
                context
                    .space
                    .typed_get_mut(list.buf + dst * size_of::<Value>()),
                len,
            );
        }
        Ok(())
    }
}
