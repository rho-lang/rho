use std::{mem::take, ptr::copy_nonoverlapping, slice, str};

use thiserror::Error;

use crate::{
    asset::{Asset, BlockId},
    code::{CaptureSource, Instr, InstrIndex, Op2},
    space::{OutOfSpace, Space, SpaceAddr},
    typing::{RecordLayout, TypeId, TypeRegistry},
};

pub struct Eval {
    space: Space,
    registry: TypeRegistry,
    task_addr: SpaceAddr,
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

#[derive(Debug)]
struct Task {
    frames: [Frame; 16],
    num_frame: usize,
    values_addr: SpaceAddr,
    values_len: usize,
    values_cap: usize,
}

#[derive(Debug)]
struct Frame {
    closure: Closure,
    instr_pointer: InstrIndex,
    value_offset: usize,
}

impl Default for Task {
    fn default() -> Self {
        Self {
            frames: [(); 16].map(|_| Frame {
                closure: Closure::new(u32::MAX),
                instr_pointer: 0,
                value_offset: 0,
            }),
            num_frame: 0,
            values_addr: SpaceAddr::MAX,
            values_len: 0,
            values_cap: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ValuesView(SpaceAddr);

impl ValuesView {
    fn offset(self, offset: usize) -> Self {
        Self(self.0 + offset * size_of::<Value>())
    }

    unsafe fn load(&self, index: usize, space: &Space) -> Value {
        *unsafe { space.typed_get(self.0 + index * size_of::<Value>()) }
    }

    unsafe fn store(&self, index: usize, value: Value, space: &mut Space) {
        unsafe { space.typed_write(self.0 + index * size_of::<Value>(), value) }
    }
}

struct TaskView(SpaceAddr);

impl TaskView {
    unsafe fn get<'a>(&self, space: &'a Space) -> &'a Task {
        unsafe { space.typed_get::<Task>(self.0) }
    }

    unsafe fn get_mut<'a>(&self, space: &'a mut Space) -> &'a mut Task {
        unsafe { space.typed_get_mut::<Task>(self.0) }
    }

    unsafe fn frame<'a>(&self, space: &'a Space) -> &'a Frame {
        let task = unsafe { self.get(space) };
        &task.frames[task.num_frame - 1]
    }

    unsafe fn frame_mut<'a>(&self, space: &'a mut Space) -> &'a mut Frame {
        let task = unsafe { self.get_mut(space) };
        &mut task.frames[task.num_frame - 1]
    }

    unsafe fn frame_values(&self, space: &Space) -> ValuesView {
        let task = unsafe { self.get(space) };
        ValuesView(task.values_addr).offset(task.frames[task.num_frame - 1].value_offset)
    }
}

#[derive(Error, Debug)]
#[error("arity error: accept {expected} arguments, but got {actual}")]
pub struct ArityError {
    pub expected: usize,
    pub actual: usize,
}

impl TaskView {
    unsafe fn push_frame(
        &self,
        closure: Closure,
        args: &[Value],
        space: &mut Space,
        asset: &Asset,
    ) -> Result<(), ExecuteError> {
        let block = asset.get_block(closure.block_id);
        if args.len() != block.num_param {
            return Err(ArityError {
                expected: block.num_param,
                actual: args.len(),
            })?;
        }

        let mut task = unsafe { self.get_mut(space) };
        assert!(
            task.num_frame < 16,
            "call stack deeper than 16 frames is not supported"
        );

        let values_addr = task.values_addr;
        let values_len = task.values_len;
        if values_len + block.num_value > task.values_cap {
            let new_cap = (values_len + block.num_value).max(task.values_cap * 2);
            let new_values_addr = space.alloc(size_of::<Value>() * new_cap, align_of::<Value>())?;
            if values_len > 0 {
                unsafe {
                    copy_nonoverlapping(
                        space.typed_get::<Value>(values_addr),
                        space.typed_get_mut(new_values_addr),
                        values_len,
                    )
                }
            }
            task = unsafe { self.get_mut(space) };
            task.values_addr = new_values_addr;
            task.values_cap = new_cap
        }
        task.frames[task.num_frame] = Frame {
            closure,
            instr_pointer: 0,
            value_offset: values_len,
        };
        task.num_frame += 1;
        task.values_len += block.num_value;

        let values = unsafe { self.frame_values(space) };
        for (i, &arg) in args.iter().enumerate() {
            unsafe { values.store(i, arg, space) }
        }
        Ok(())
    }
}

impl Eval {
    pub fn new(registry: TypeRegistry) -> Self {
        Self {
            space: Space::new(1 << 20),
            registry,
            task_addr: SpaceAddr::MAX,
        }
    }
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
    #[error("unreachable: {0}")]
    Unreachable(&'static str),
}

impl Eval {
    pub unsafe fn run(mut self, main: Closure, asset: &Asset) -> Result<(), ExecuteError> {
        let task_value = Value::alloc_task(&mut self.space)?;
        self.task_addr = task_value.addr();
        unsafe { TaskView(self.task_addr).push_frame(main, &[], &mut self.space, asset)? }
        while let Err(err) = unsafe { self.run_impl(asset) } {
            if let ExecuteError::Space(OutOfSpace(requested_size)) = err {
                self.space
                    .copy_collect(OutOfSpace(requested_size), self.task_addr)
            } else {
                return Err(err);
            }
        }
        Ok(())
    }

    unsafe fn run_impl(&mut self, asset: &Asset) -> Result<(), ExecuteError> {
        let mut return_epilogue = None;
        'nonlocal_control: loop {
            let task = TaskView(self.task_addr);
            let block = asset.get_block(unsafe { task.frame(&self.space) }.closure.block_id);
            let frame_values = unsafe { task.frame_values(&self.space) };

            if let Some(value) = take(&mut return_epilogue) {
                let &Instr::Call(dst, ..) =
                    &block.instrs[unsafe { task.frame(&self.space) }.instr_pointer]
                else {
                    unreachable!()
                };
                unsafe { frame_values.store(dst, value, &mut self.space) }
                unsafe { task.frame_mut(&mut self.space) }.instr_pointer += 1 // current Call instruction is done
            }

            'local_control: loop {
                // optimize for sequentially execute instructions
                let mut instr_pointer = unsafe { task.frame(&self.space) }.instr_pointer;
                for instr in &block.instrs[instr_pointer..] {
                    // invariant: instr = block.instrs[frame.instr_pointer]
                    // at the end of this loop, instr_pointer is incremented to maintain this
                    // invariant. the increment is at the end for re-executing failed instructions
                    // in recoverable cases like OutOfSpace. also, every adjustment of instr_pointer
                    // (for control flow) is followed by a `break` or `continue` of the outer
                    // 'local_control
                    tracing::trace!(%instr_pointer, %block.name, ?instr, "execute");
                    match instr {
                        Instr::Call(_, closure, args) => {
                            let closure_value = unsafe { frame_values.load(*closure, &self.space) };
                            let closure = closure_value.load_closure(&self.space)?;
                            let arg_values = args
                                .iter()
                                .map(|arg| unsafe { frame_values.load(*arg, &self.space) })
                                .collect::<Vec<_>>();
                            unsafe { task.frame_mut(&mut self.space) }.instr_pointer =
                                instr_pointer;
                            unsafe {
                                task.push_frame(closure, &arg_values, &mut self.space, asset)?
                            }
                            continue 'nonlocal_control;
                        }
                        Instr::Return(index) => {
                            let return_value = unsafe { frame_values.load(*index, &self.space) };
                            let task = unsafe { task.get_mut(&mut self.space) };
                            task.values_len -= block.num_value;
                            task.num_frame -= 1;
                            if task.num_frame == 0 {
                                return_value.load_unit()?; // should we make a dedicated error variant?
                                break 'nonlocal_control Ok(());
                            }
                            let replaced = return_epilogue.replace(return_value);
                            assert!(replaced.is_none());
                            continue 'nonlocal_control;
                        }

                        Instr::Switch(index) => {
                            let task_value = unsafe { frame_values.load(*index, &self.space) };
                            task_value.ensure_type(TypeId::TASK)?;
                            // after switching back (if any), current Switch instruction is done
                            unsafe { task.frame_mut(&mut self.space) }.instr_pointer =
                                instr_pointer + 1;
                            self.task_addr = task_value.addr();
                            continue 'nonlocal_control;
                        }

                        Instr::Jump(instr_index, cond) => {
                            // not calling Option::map because load_type_id may throw
                            if match cond {
                                None => true,
                                Some(cond) => {
                                    let target =
                                        unsafe { frame_values.load(cond.pattern, &self.space) }
                                            .load_type_id()?;
                                    unsafe { frame_values.load(cond.scrutinee, &self.space) }
                                        .type_id()
                                        == target
                                }
                            } {
                                unsafe { task.frame_mut(&mut self.space) }.instr_pointer =
                                    *instr_index;
                                continue 'local_control;
                            }
                        }

                        Instr::MakeClosure(dst, block_id) => {
                            let closure_value =
                                Value::alloc_closure(Closure::new(*block_id), &mut self.space)?;
                            unsafe { frame_values.store(*dst, closure_value, &mut self.space) }
                        }
                        Instr::Promote(index) => {
                            let value = unsafe { frame_values.load(*index, &self.space) };
                            let cell_value = Value::alloc_cell(value, &mut self.space);
                            unsafe { frame_values.store(*index, cell_value?, &mut self.space) }
                        }
                        Instr::Capture(dst, source) => {
                            let addr = match source {
                                CaptureSource::Original(index) => {
                                    let captured_value =
                                        unsafe { frame_values.load(*index, &self.space) };
                                    captured_value.ensure_type(TypeId::CELL).unwrap();
                                    captured_value.addr()
                                }
                                CaptureSource::Transitive(index) => {
                                    unsafe { task.frame(&self.space) }.closure.captured[*index]
                                }
                            };
                            unsafe { frame_values.load(*dst, &self.space) }
                                .get_closure_mut(&mut self.space)
                                .unwrap()
                                .capture(addr)
                        }
                        // the 0 position corresponds to Ref layout
                        Instr::GetCaptured(dst, captured_index) => {
                            let cell = ValuesView(
                                unsafe { task.frame(&self.space) }.closure.captured
                                    [*captured_index],
                            );
                            let value = unsafe { cell.load(0, &self.space) };
                            unsafe { frame_values.store(*dst, value, &mut self.space) }
                        }
                        Instr::SetCaptured(captured_index, index) => {
                            let value = unsafe { frame_values.load(*index, &self.space) };
                            let cell = ValuesView(
                                unsafe { task.frame(&self.space) }.closure.captured
                                    [*captured_index],
                            );
                            unsafe { cell.store(0, value, &mut self.space) }
                        }
                        Instr::Demote(index) => {
                            let cell_value = unsafe { frame_values.load(*index, &self.space) };
                            cell_value.ensure_type(TypeId::CELL).unwrap();
                            let value =
                                unsafe { ValuesView(cell_value.addr()).load(0, &self.space) };
                            unsafe { frame_values.store(*index, value, &mut self.space) }
                        }
                        Instr::SetPromoted(dst, index) => {
                            let value = unsafe { frame_values.load(*index, &self.space) };
                            let dst_value = unsafe { frame_values.load(*dst, &mut self.space) };
                            dst_value.ensure_type(TypeId::CELL).unwrap();
                            unsafe { ValuesView(dst_value.addr()).store(0, value, &mut self.space) }
                        }

                        Instr::MakeRecordType(dst, layout) => {
                            let type_id = self.registry.add_record_type(layout.clone());
                            unsafe {
                                frame_values.store(
                                    *dst,
                                    Value::new_type_id(type_id),
                                    &mut self.space,
                                )
                            }
                        }
                        Instr::MakeRecord(dst, type_id, attrs) => {
                            let type_id = unsafe { frame_values.load(*type_id, &self.space) }
                                .load_type_id()?;
                            let Some(RecordLayout(layout)) =
                                self.registry.get_record_layout(type_id)
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
                                record_attrs[pos] =
                                    unsafe { frame_values.load(*index, &self.space) };
                            }
                            if attrs.len() != layout.len() {
                                return Err(ExecuteError::MissingAttr);
                            }
                            let record_value =
                                Value::alloc_record(type_id, record_attrs, &mut self.space)?;
                            unsafe { frame_values.store(*dst, record_value, &mut self.space) }
                        }
                        Instr::GetAttr(dst, record, attr) => {
                            let record_value = unsafe { frame_values.load(*record, &self.space) };
                            let Some(RecordLayout(layout)) =
                                self.registry.get_record_layout(record_value.type_id())
                            else {
                                return Err(ExecuteError::NotRecord);
                            };
                            let Some(pos) =
                                layout.iter().position(|layout_attr| layout_attr == attr)
                            else {
                                return Err(ExecuteError::UnexpectedAttr(
                                    asset.get_string(*attr).into(),
                                ));
                            };
                            let value =
                                unsafe { ValuesView(record_value.addr()).load(pos, &self.space) };
                            unsafe { frame_values.store(*dst, value, &mut self.space) }
                        }
                        Instr::SetAttr(record, attr, index) => {
                            let record_value = unsafe { frame_values.load(*record, &self.space) };
                            let Some(RecordLayout(layout)) =
                                self.registry.get_record_layout(record_value.type_id())
                            else {
                                return Err(ExecuteError::NotRecord);
                            };
                            let Some(pos) =
                                layout.iter().position(|layout_attr| layout_attr == attr)
                            else {
                                return Err(ExecuteError::UnexpectedAttr(
                                    asset.get_string(*attr).into(),
                                ));
                            };
                            let value = unsafe { frame_values.load(*index, &self.space) };
                            unsafe {
                                ValuesView(record_value.addr()).store(pos, value, &mut self.space)
                            }
                        }

                        Instr::MakeUnit(dst) => unsafe {
                            frame_values.store(*dst, Value::new_unit(), &mut self.space)
                        },
                        Instr::MakeString(dst, literal) => {
                            let string_value = Value::alloc_string(literal, &mut self.space)?;
                            unsafe { frame_values.store(*dst, string_value, &mut self.space) }
                        }
                        Instr::MakeI32(dst, value) => unsafe {
                            frame_values.store(*dst, Value::new_int32(*value), &mut self.space)
                        },

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
                                    let a = unsafe { frame_values.load(*a, &self.space) }
                                        .load_int32()?;
                                    let b = unsafe { frame_values.load(*b, &self.space) }
                                        .load_int32()?;
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
                            unsafe { frame_values.store(*dst, dst_value, &mut self.space) }
                        }

                        Instr::Copy(dst, src) => {
                            let src_value = unsafe { frame_values.load(*src, &self.space) };
                            unsafe { frame_values.store(*dst, src_value, &mut self.space) }
                        }
                        Instr::Intrinsic(intrinsic, indexes) => {
                            unsafe { intrinsic(frame_values, indexes, self, asset) }?
                        }
                        Instr::Unreachable(msg) => {
                            return Err(ExecuteError::Unreachable(msg));
                        }
                    }

                    instr_pointer += 1
                }
                unreachable!("block should terminate with Return")
            }
        }
    }
}

#[derive(Error, Debug)]
#[error("type error: expected {expected:?}, actual {actual:?}")]
pub struct TypeError {
    pub expected: TypeId,
    pub actual: TypeId,
}

struct Unit;

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

    // cell type, represented with a single SpaceAddr
    fn alloc_cell(value: Value, space: &mut Space) -> Result<Self, OutOfSpace> {
        let addr = space.typed_alloc::<Value>()?;
        unsafe { space.typed_write(addr, value) }
        Ok(Self::new(TypeId::CELL, addr))
    }
    // access to cell is performed directly with SpaceAddr, not going through Value

    fn alloc_task(space: &mut Space) -> Result<Value, OutOfSpace> {
        let addr = space.typed_alloc::<Task>()?;
        unsafe { space.typed_write(addr, Task::default()) }
        Ok(Value::new(TypeId::TASK, addr))
    }

    // unit type
    fn new_unit() -> Self {
        Self::new_atom(TypeId::UNIT)
    }

    fn load_unit(&self) -> Result<Unit, TypeError> {
        self.ensure_type(TypeId::UNIT)?;
        Ok(Unit)
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
        eval::{Eval, ExecuteError, TaskView, TypeError, Value, ValuesView},
        space::{OutOfSpace, Space, SpaceAddr},
        typing::{RecordLayout, TypeId, TypeRegistry},
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
            ("park", park),
            ("task_new", task_new),
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

    unsafe fn task_new(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        asset: &Asset,
    ) -> Result<(), ExecuteError> {
        let closure =
            unsafe { values.load(indexes[1], &context.space) }.load_closure(&context.space)?;
        let task_value = Value::alloc_task(&mut context.space)?;
        unsafe { TaskView(task_value.addr()).push_frame(closure, &[], &mut context.space, asset)? };
        unsafe { values.store(indexes[0], task_value, &mut context.space) }
        Ok(())
    }

    fn park(_: ValuesView, _: &[ValueIndex], _: &mut Eval, _: &Asset) -> Result<(), ExecuteError> {
        std::thread::park();
        Ok(())
    }

    struct String {
        buf: SpaceAddr,
        len: usize,
        // TODO add cap
    }

    impl Value {
        pub fn alloc_string(literal: &str, space: &mut Space) -> Result<Self, OutOfSpace> {
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
    }

    unsafe fn trace(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        asset: &Asset,
    ) -> Result<(), ExecuteError> {
        let value = unsafe { values.load(indexes[0], &context.space) };

        fn format_value(
            value: Value,
            top: bool,
            space: &Space,
            registry: &TypeRegistry,
            asset: &Asset,
        ) -> std::string::String {
            if let Ok(message) = value.get_str(space) {
                message.into()
            } else if let Ok(int32) = value.load_int32() {
                int32.to_string()
            } else if let Some(RecordLayout(attrs)) = registry.get_record_layout(value.type_id()) {
                let attrs = if top {
                    attrs
                        .iter()
                        .enumerate()
                        .map(|(i, &attr)| {
                            let attr_value = unsafe { ValuesView(value.addr()).load(i, space) };
                            format!(
                                "{} = {}",
                                asset.get_string(attr),
                                format_value(attr_value, false, space, registry, asset)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                } else {
                    "...".into()
                };
                format!("record({:?})[{attrs}]", value.type_id())
            } else {
                "<unknown>".into() // TODO
            }
        }

        tracing::info!(
            "{}",
            format_value(value, true, &context.space, &context.registry, asset)
        );
        Ok(())
    }

    unsafe fn type_object(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let id = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
        unsafe {
            values.store(
                indexes[0],
                Value::new_type_id(TypeId(id as _)),
                &mut context.space,
            )
        }
        Ok(())
    }

    thread_local!(static START: std::time::Instant = std::time::Instant::now());
    unsafe fn time_since_start(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let duration = START.with(|start| start.elapsed());
        assert!(
            duration.as_secs() < i32::MAX as _,
            "program has been running for too long"
        );
        unsafe {
            values.store(
                indexes[0],
                Value::new_int32(duration.as_secs() as _),
                &mut context.space,
            );
            values.store(
                indexes[1],
                Value::new_int32(duration.subsec_nanos() as _),
                &mut context.space,
            )
        }
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

    unsafe fn list_new(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let cap = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
        if cap < 0 {
            todo!()
        }
        let list_value = Value::alloc_list(cap as _, &mut context.space)?;
        unsafe { values.store(indexes[0], list_value, &mut context.space) }
        Ok(())
    }

    // list_push is implemented from lang side, with the following intrinsics
    unsafe fn list_len(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let list = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
        unsafe {
            values.store(
                indexes[0],
                Value::new_int32(list.len as _),
                &mut context.space,
            )
        }
        Ok(())
    }

    unsafe fn list_set_len(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let len = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
        let list =
            unsafe { values.load(indexes[0], &context.space) }.get_list_mut(&mut context.space)?;
        assert!(len >= 0);
        assert!(len as usize <= list.cap);
        list.len = len as _;
        Ok(())
    }

    unsafe fn list_cap(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let list = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
        unsafe {
            values.store(
                indexes[0],
                Value::new_int32(list.cap as _),
                &mut context.space,
            )
        }
        Ok(())
    }

    // while this can be achieved by per-value load + store, expect a performance
    // gap to necessitate
    unsafe fn list_copy(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let src = unsafe { values.load(indexes[0], &context.space) }.get_list(&context.space)?;
        let dst = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
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

    unsafe fn list_load(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let list = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
        let pos = unsafe { values.load(indexes[2], &context.space) }.load_int32()?;
        assert!(pos >= 0);
        assert!((pos as usize) < list.len); // avoid exposing garbage value to lang
        let value = unsafe { ValuesView(list.buf).load(pos as _, &context.space) };
        unsafe { values.store(indexes[0], value, &mut context.space) }
        Ok(())
    }

    unsafe fn list_store(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let list = unsafe { values.load(indexes[0], &context.space) }.get_list(&context.space)?;
        let pos = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
        assert!(pos >= 0);
        assert!((pos as usize) < list.cap); // as long as not overflowing it is fine
        let value = unsafe { values.load(indexes[2], &context.space) };
        unsafe { ValuesView(list.buf).store(pos as _, value, &mut context.space) };
        Ok(())
    }

    // similar to list_copy but for a performant list_insert and list_remove
    unsafe fn list_copy_within(
        values: ValuesView,
        indexes: &[ValueIndex],
        context: &mut Eval,
        _: &Asset,
    ) -> Result<(), ExecuteError> {
        let list = unsafe { values.load(indexes[0], &context.space) }.get_list(&context.space)?;
        let src = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
        let len = unsafe { values.load(indexes[2], &context.space) }.load_int32()?;
        let dst = unsafe { values.load(indexes[3], &context.space) }.load_int32()?;
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
