use std::{slice, str};

use thiserror::Error;

use crate::{
    asset::{Asset, BlockId},
    code::{CaptureSource, Instr, InstrIndex},
    oracle::{Oracle, OracleError},
    sched::{NotifyToken, Sched},
    space::{Space, SpaceAddr},
    task::Task,
    typing::{RecordLayout, TypeId, TypeRegistry},
};

// optimization plan: pack Value into a u64 word. 24 bits for `type_id` and 40
// bits for `data`. assuming Space alignment >= 8 (or else a 4 byte data can be
// stored inline), 40 bits address space = 2^(40+3) byte or 8 TB heap space

const _: () = assert!(size_of::<SpaceAddr>() == size_of::<usize>());
const _: () = assert!(align_of::<SpaceAddr>() == align_of::<usize>());
#[derive(Debug, Clone, Copy)]
pub struct Value {
    type_id: TypeId,
    data: usize, // embedded data for inline types, SpaceAddr pointing to actual data for others
}

impl Default for Value {
    fn default() -> Self {
        Self {
            type_id: TypeId::VACANT,
            data: SpaceAddr::MAX,
        }
    }
}

struct Unit;

struct String {
    buf: SpaceAddr,
    len: usize,
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

#[derive(Debug, Clone, Copy)]
struct Future(NotifyToken);

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

pub enum ExecuteStatus {
    Waiting(NotifyToken),
    Exited,
}

#[derive(Error, Debug)]
pub enum ExecuteError {
    #[error("{0}")]
    Type(#[from] TypeError),
    #[error("{0}")]
    Arity(#[from] ArityError),
    #[error("{0}")]
    Oracle(#[from] OracleError),
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
        space: &mut Space,
        registry: &mut TypeRegistry,
        sched: &mut Sched,
        oracle: &mut Oracle,
        asset: &Asset,
    ) -> Result<ExecuteStatus, ExecuteError> {
        // the contract here is `execute` should only be called after calling `init`,
        // and should not be called again after returning Exited
        let mut frame = self.frames.last_mut().expect("last frame exists");
        let block = asset.get_block(frame.closure.block_id);
        'control: loop {
            // optimize for sequentially execute instructions
            for instr in &block.instrs[frame.instr_pointer..] {
                tracing::trace!(%frame.instr_pointer, %block.name, ?instr, "execute");
                frame.instr_pointer += 1;
                match instr {
                    Instr::Jump(instr_index, cond) => {
                        if match cond {
                            None => true,
                            Some(cond) => {
                                let target = frame.values[cond.pattern].load_type_id()?;
                                frame.values[cond.scrutinee].type_id == target
                            }
                        } {
                            frame.instr_pointer = *instr_index;
                            continue 'control;
                        }
                    }

                    Instr::Intrinsic(intrinsic, indexes) => {
                        intrinsic(&mut frame.values, indexes, space, oracle)?
                    }
                    Instr::Copy(dst, src) => {
                        let src_value = frame.values[*src];
                        frame.values[*dst] = src_value
                    }

                    Instr::MakeUnit(dst) => frame.values[*dst] = Value::new_unit(),
                    Instr::MakeString(dst, literal) => {
                        frame.values[*dst] = Value::alloc_string(literal, space)
                    }

                    Instr::MakeFuture(dst) => {
                        let future = Future(sched.alloc_notify_token());
                        frame.values[*dst] = Value::alloc_future(future, space)
                    }
                    Instr::Wait(future) => {
                        let Future(notify_token) = frame.values[*future].load_future(space)?;
                        return Ok(ExecuteStatus::Waiting(notify_token));
                    }
                    Instr::Notify(future) => {
                        let Future(notify_token) = frame.values[*future].load_future(space)?;
                        sched.notify(notify_token)
                    }

                    Instr::MakeClosure(dst, block_id) => {
                        let closure_value = Value::alloc_closure(Closure::new(*block_id), space);
                        frame.values[*dst] = closure_value
                    }
                    Instr::Promote(index) => {
                        frame.values[*index] = Value::alloc_cell(frame.values[*index], space)
                    }
                    Instr::Capture(dst, source) => {
                        let addr = match source {
                            CaptureSource::Owning(index) => {
                                let captured_value = frame.values[*index];
                                captured_value.ensure_type(TypeId::CELL).unwrap();
                                captured_value.data
                            }
                            CaptureSource::Transitive(index) => frame.closure.captured[*index],
                        };
                        frame.values[*dst]
                            .get_closure_mut(space)
                            .unwrap()
                            .capture(addr)
                    }
                    // the 0 position corresponds to Ref layout
                    Instr::GetCaptured(dst, captured_index) => {
                        frame.values[*dst] = unsafe {
                            load_record_attr(space, frame.closure.captured[*captured_index], 0)
                        }
                    }
                    Instr::SetCaptured(captured_index, index) => unsafe {
                        store_record_attr(
                            space,
                            frame.closure.captured[*captured_index],
                            0,
                            frame.values[*index],
                        )
                    },
                    Instr::Demote(index) => {
                        let value = frame.values[*index];
                        value.ensure_type(TypeId::CELL).unwrap();
                        frame.values[*index] = unsafe { load_record_attr(space, value.data, 0) }
                    }
                    Instr::SetPromoted(dst, src) => {
                        let dst_value = frame.values[*dst];
                        dst_value.ensure_type(TypeId::CELL).unwrap();
                        unsafe { store_record_attr(space, dst_value.data, 0, frame.values[*src]) }
                    }
                    Instr::Spawn(closure) => {
                        let mut eval = Self::new();
                        eval.init(frame.values[*closure].load_closure(space)?, asset)?;
                        sched.spawn(Task::new(eval));
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
                        self.push_frame(closure_value.load_closure(space)?, &arg_values, asset)?;
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
                        continue 'control;
                    }

                    Instr::MakeRecordType(dst, layout) => {
                        let type_id = registry.add_record_type(layout.clone());
                        frame.values[*dst] = Value::new_type_id(type_id)
                    }
                    Instr::MakeRecord(dst, type_id, attrs) => {
                        let type_id = frame.values[*type_id].load_type_id()?;
                        let Some(RecordLayout(layout)) = registry.get_record_layout(type_id) else {
                            return Err(ExecuteError::NotRecord);
                        };
                        let mut record_attrs = vec![Value::default(); layout.len()];
                        for (attr, index) in attrs {
                            let Some(pos) =
                                layout.iter().position(|layout_attr| layout_attr == attr)
                            else {
                                return Err(ExecuteError::UnexpectedAttr("TODO".into()));
                            };
                            record_attrs[pos] = frame.values[*index]
                        }
                        if attrs.len() != layout.len() {
                            return Err(ExecuteError::MissingAttr);
                        }
                        frame.values[*dst] = Value::alloc_record(type_id, record_attrs, space)
                    }
                    Instr::GetAttr(dst, record, attr) => {
                        let record_value = &frame.values[*record];
                        let Some(RecordLayout(layout)) =
                            registry.get_record_layout(record_value.type_id)
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
                            unsafe { load_record_attr(space, record_value.data, pos) }
                    }
                    Instr::SetAttr(record, attr, index) => {
                        let record_value = &frame.values[*record];
                        let Some(RecordLayout(layout)) =
                            registry.get_record_layout(record_value.type_id)
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
                            store_record_attr(space, record_value.data, pos, frame.values[*index])
                        }
                    }
                }
            }
            unreachable!("block should terminate with Return")
        }
    }
}

unsafe fn load_record_attr(space: &Space, addr: SpaceAddr, pos: usize) -> Value {
    *unsafe { space.typed_get(addr + pos * size_of::<Value>()) }
}

unsafe fn store_record_attr(space: &mut Space, addr: SpaceAddr, pos: usize, value: Value) {
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
        if self.type_id != expected {
            return Err(TypeError {
                expected,
                actual: self.type_id,
            });
        }
        Ok(())
    }

    fn load_inline(&self) -> u64 {
        self.data as _
    }

    // helper methods for essential types
    // new_x        (X) -> Self                     construct inline type
    // alloc_x      (X, &mut Space) -> Self         construct heap-allocated type
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
    // for read-only types (e.g., Future), only `load_x` and/or `get_x` may be
    // provided. for heap-allocated mutable types, which of the four access methods
    // are provided is based on an on-demand manner and depends on the access
    // pattern. For example, Closure provides `get_closure_mut` for implementing
    // Capture and `load_closure` for implementing Call, String provides
    // `get_string` and `get_string_mut` for implementing intrinsics, etc.

    // TypeId
    fn new_type_id(type_id: TypeId) -> Self {
        Self {
            type_id: TypeId::TYPE_ID,
            data: type_id.0 as _,
        }
    }

    fn load_type_id(&self) -> Result<TypeId, TypeError> {
        self.ensure_type(TypeId::TYPE_ID)?;
        Ok(TypeId(self.load_inline() as _))
    }

    // Closure
    fn alloc_closure(closure: Closure, space: &mut Space) -> Self {
        let addr = space.typed_alloc::<Closure>();
        unsafe { space.typed_write(addr, closure) }
        Self {
            type_id: TypeId::CLOSURE,
            data: addr,
        }
    }

    fn load_closure(&self, space: &Space) -> Result<Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(*unsafe { space.typed_get(self.data) })
    }

    fn get_closure_mut<'a>(&self, space: &'a mut Space) -> Result<&'a mut Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(unsafe { space.typed_get_mut(self.data) })
    }

    // cell type
    fn alloc_cell(value: Value, space: &mut Space) -> Self {
        let addr = space.typed_alloc::<Value>();
        unsafe { space.typed_write(addr, value) }
        Self {
            type_id: TypeId::CELL,
            data: addr,
        }
    }
    // access to cell is performed directly with SpaceAddr, not going through Value

    // Future
    fn alloc_future(future: Future, space: &mut Space) -> Self {
        let addr = space.typed_alloc::<Future>();
        unsafe { space.typed_write(addr, future) };
        Self {
            type_id: TypeId::FUTURE,
            data: addr,
        }
    }

    fn load_future(&self, space: &Space) -> Result<Future, TypeError> {
        self.ensure_type(TypeId::FUTURE)?;
        Ok(*unsafe { space.typed_get(self.data) })
    }

    // unit type
    fn new_unit() -> Self {
        Self {
            type_id: TypeId::UNIT,
            data: SpaceAddr::MAX,
        }
    }

    fn load_unit(&self) -> Result<Unit, TypeError> {
        self.ensure_type(TypeId::UNIT)?;
        Ok(Unit)
    }

    // String
    fn alloc_string(literal: &str, space: &mut Space) -> Self {
        let len = literal.len();
        let addr = space.typed_alloc::<String>();
        let buf = space.alloc(len, 1);
        space.get_mut(buf, len).copy_from_slice(literal.as_bytes());
        unsafe { space.typed_write(addr, String { buf, len }) }
        Self {
            type_id: TypeId::STRING,
            data: addr,
        }
    }

    fn get_string<'a>(&self, space: &'a Space) -> Result<&'a str, TypeError> {
        self.ensure_type(TypeId::STRING)?;
        let string = unsafe { space.typed_get::<String>(self.data) };
        Ok(unsafe { str::from_utf8_unchecked(space.get(string.buf, string.len)) })
    }

    // record types
    fn alloc_record(type_id: TypeId, attrs: Vec<Value>, space: &mut Space) -> Self {
        let addr = space.alloc(size_of::<Value>() * attrs.len(), align_of::<Value>());
        let buf = space
            .get_mut(addr, size_of::<Value>() * attrs.len())
            .as_mut_ptr()
            .cast::<Value>();
        let record_attrs = unsafe { slice::from_raw_parts_mut(buf, attrs.len()) };
        for (dst, src) in record_attrs.iter_mut().zip(attrs) {
            *dst = src
        }
        Self {
            type_id,
            data: addr,
        }
    }
}

pub mod intrinsics {
    use thiserror::Error;

    use crate::{
        code::ValueIndex,
        eval::{ExecuteError, Value},
        oracle::Oracle,
        space::Space,
    };

    use super::Future;

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

    pub fn trace(
        values: &mut [Value],
        indexes: &[ValueIndex],
        space: &mut Space,
        _: &mut Oracle,
    ) -> Result<(), ExecuteError> {
        let message = values[indexes[0]].get_string(space)?;
        tracing::info!(message);
        Ok(())
    }

    pub fn notify_after(
        values: &mut [Value],
        indexes: &[ValueIndex],
        space: &mut Space,
        oracle: &mut Oracle,
    ) -> Result<(), ExecuteError> {
        let Future(notify_token) = values[indexes[0]].load_future(space)?;
        let duration = values[indexes[1]]
            .get_string(space)?
            .parse::<humantime::Duration>()
            .map_err(Error::ParseDuration)?
            .into();
        oracle.notify_after(notify_token, duration)?;
        Ok(())
    }
}
