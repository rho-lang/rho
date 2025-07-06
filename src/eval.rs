use std::{alloc::Layout, mem::take, str};

use thiserror::Error;

use crate::{
    asset::{Asset, BlockId},
    code::{CaptureSource, Instr, InstrIndex, Op2},
    space::{OutOfSpace, Space, SpaceAddr},
    typing::{RecordLayout, TypeId, TypeRegistry},
};

pub mod intrinsics;

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
    data: ValueData,
}

#[derive(Debug, Clone, Copy)]
enum ValueData {
    Nothing,
    Inline(u64),
    Addr(SpaceAddr),
}

// helpers to maintain compatibility after the optimization is applied
impl Value {
    fn new(type_id: TypeId, addr: SpaceAddr) -> Self {
        Self {
            type_id,
            data: ValueData::Addr(addr),
        }
    }

    fn new_inline(type_id: TypeId, data: u64) -> Self {
        Self {
            type_id,
            data: ValueData::Inline(data),
        }
    }

    fn new_atom(type_id: TypeId) -> Self {
        Self {
            type_id,
            data: ValueData::Nothing,
        }
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }

    fn data(&self) -> u64 {
        if let ValueData::Inline(data) = self.data {
            data
        } else {
            unimplemented!("data is not inline");
        }
    }

    fn addr(&self) -> SpaceAddr {
        if let ValueData::Addr(addr) = self.data {
            addr
        } else {
            unimplemented!("data is not an address");
        }
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
            captured: [Default::default(); 16],
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
            values_addr: Default::default(),
            values_len: 0,
            values_cap: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ValuesView(SpaceAddr);

impl ValuesView {
    unsafe fn offset(self, offset: usize) -> Self {
        Self(unsafe { self.0.add(offset * size_of::<Value>()) })
    }

    unsafe fn load(&self, index: usize, space: &Space) -> Value {
        *unsafe { space.typed_get(self.0.add(index * size_of::<Value>())) }
    }

    unsafe fn store(&self, index: usize, value: Value, space: &mut Space) {
        unsafe { space.typed_write(self.0.add(index * size_of::<Value>()), value) }
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
        unsafe { ValuesView(task.values_addr).offset(task.frames[task.num_frame - 1].value_offset) }
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
            // let new_values_addr = space.alloc(Layout::array::<Value>(new_cap).unwrap())?;
            // if values_len > 0 {
            //     unsafe {
            //         copy_nonoverlapping(
            //             space.typed_get::<Value>(values_addr),
            //             space.typed_get_mut(new_values_addr),
            //             values_len,
            //         )
            //     }
            // }
            let new_values_addr =
                unsafe { space.alloc_copy::<Value>(new_cap, values_addr, values_len)? };
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
            space: Space::new(None),
            registry,
            task_addr: Default::default(),
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
                            let record_value =
                                Value::alloc_record(type_id, layout.len(), &mut self.space)?;
                            let attr_values = ValuesView(record_value.addr());
                            for (attr, index) in attrs {
                                let Some(pos) =
                                    layout.iter().position(|layout_attr| layout_attr == attr)
                                else {
                                    return Err(ExecuteError::UnexpectedAttr(
                                        asset.get_string(*attr).into(),
                                    ));
                                };
                                let value = unsafe { frame_values.load(*index, &self.space) };
                                unsafe { attr_values.store(pos, value, &mut self.space) }
                            }
                            if attrs.len() != layout.len() {
                                return Err(ExecuteError::MissingAttr);
                            }
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
        num_attr: usize,
        space: &mut Space,
    ) -> Result<Self, OutOfSpace> {
        let addr = space.alloc(Layout::array::<Value>(num_attr).unwrap())?;
        Ok(Self::new(type_id, addr))
    }
}
