use std::{alloc::Layout, mem::take, ptr::copy_nonoverlapping, str};

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

impl Eval {
    pub fn new(registry: TypeRegistry) -> Self {
        Self {
            space: Space::new(None),
            registry,
            task_addr: Default::default(),
        }
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
    data: ValueData,
}

#[derive(Debug, Clone, Copy)]
enum ValueData {
    Phantom,
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

    fn new_phantom(type_id: TypeId) -> Self {
        Self {
            type_id,
            data: ValueData::Phantom,
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
        Self::new_phantom(TypeId::VACANT)
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

#[derive(Error, Debug)]
#[error("arity error: accept {expected} arguments, but got {actual}")]
pub struct ArityError {
    pub expected: usize,
    pub actual: usize,
}

impl Task {
    fn push_frame(
        &mut self,
        closure: Closure,
        args: &[Value],
        space: &mut Space,
        asset: &Asset,
    ) -> Result<(), ExecuteError> {
        let block = asset.get_block(closure.block_id);
        if args.len() != block.num_param {
            Err(ArityError {
                expected: block.num_param,
                actual: args.len(),
            })?
        }

        assert!(
            self.num_frame < 16,
            "call stack deeper than 16 frames is not supported"
        );

        if self.values_len + block.num_value > self.values_cap {
            let new_cap = (self.values_len + block.num_value).max(self.values_cap * 2);
            let new_values_addr = space.alloc(Layout::array::<Value>(new_cap).unwrap())?;
            if self.values_len > 0 {
                unsafe {
                    copy_nonoverlapping(
                        self.values_addr.as_ptr(),
                        new_values_addr.as_ptr(),
                        self.values_len * size_of::<Value>(),
                    )
                }
            }
            self.values_addr = new_values_addr;
            self.values_cap = new_cap;
        }

        let value_offset = self.values_len;
        self.frames[self.num_frame] = Frame {
            closure,
            instr_pointer: 0,
            value_offset,
        };
        self.num_frame += 1;
        self.values_len += block.num_value;

        for (i, &arg) in args.iter().enumerate() {
            unsafe { values_write(self.values_addr, value_offset + i, arg) }
        }
        Ok(())
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
        unsafe { self.task_addr.cast::<Task>().as_mut() }.push_frame(
            main,
            &[],
            &mut self.space,
            asset,
        )?;
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
        // modified by inter-task control, i.e., just Switch
        // too cumbersome to nest one level, just re-assign in Switch branch
        let mut task = unsafe { self.task_addr.cast::<Task>().as_mut() };
        'nonlocal_control: loop {
            // modified by inter-frame control, i.e., Call and Return
            let frame = &mut task.frames[task.num_frame - 1];
            let block = asset.get_block(frame.closure.block_id);

            // notice that task.value_addr may be modified by the push_frame in Call, so we
            // need to reconstruct these closures instead of lift to outside
            let load = |i| unsafe { values_read(task.values_addr, frame.value_offset + i) };
            let store =
                |i, value| unsafe { values_write(task.values_addr, frame.value_offset + i, value) };

            if let Some(value) = take(&mut return_epilogue) {
                let &Instr::Call(dst, ..) = &block.instrs[frame.instr_pointer] else {
                    unreachable!()
                };
                store(dst, value);
                frame.instr_pointer += 1
            }
            'local_control: loop {
                // optimize for sequentially execute instructions
                let mut instr_pointer = frame.instr_pointer;
                for instr in &block.instrs[instr_pointer..] {
                    tracing::trace!(%instr_pointer, %block.name, ?instr, "execute");
                    match instr {
                        Instr::Call(_, closure, args) => {
                            let closure = unsafe { load(*closure).load_closure() }?;
                            let arg_values = args.iter().map(|&arg| load(arg)).collect::<Vec<_>>();
                            frame.instr_pointer = instr_pointer;
                            task.push_frame(closure, &arg_values, &mut self.space, asset)?;
                            continue 'nonlocal_control;
                        }
                        Instr::Return(index) => {
                            let return_value = load(*index);
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
                            let task_value = load(*index);
                            task_value.ensure_type(TypeId::TASK)?;
                            // after switching back (if any), current Switch instruction is done
                            frame.instr_pointer = instr_pointer + 1;
                            self.task_addr = task_value.addr();
                            task = unsafe { self.task_addr.cast::<Task>().as_mut() };
                            continue 'nonlocal_control;
                        }

                        Instr::Jump(instr_index, cond) => {
                            // not calling Option::map because load_type_id may throw
                            if match cond {
                                None => true,
                                Some(cond) => {
                                    let target = load(cond.pattern).load_type_id()?;
                                    load(cond.scrutinee).type_id() == target
                                }
                            } {
                                frame.instr_pointer = *instr_index;
                                continue 'local_control;
                            }
                        }

                        Instr::MakeClosure(dst, block_id) => store(
                            *dst,
                            Value::alloc_closure(Closure::new(*block_id), &mut self.space)?,
                        ),
                        Instr::Promote(index) => {
                            store(*index, Value::alloc_cell(load(*index), &mut self.space)?)
                        }
                        Instr::Capture(dst, source) => {
                            let addr = match source {
                                CaptureSource::Original(index) => {
                                    let captured_value = load(*index);
                                    captured_value.ensure_type(TypeId::CELL).unwrap();
                                    captured_value.addr()
                                }
                                CaptureSource::Transitive(index) => frame.closure.captured[*index],
                            };
                            unsafe { load(*dst).get_closure_mut() }
                                .unwrap()
                                .capture(addr)
                        }
                        // the 0 position corresponds to cell layout
                        Instr::GetCaptured(dst, captured_index) => store(*dst, unsafe {
                            values_read(frame.closure.captured[*captured_index], 0)
                        }),
                        Instr::SetCaptured(captured_index, index) => unsafe {
                            values_write(frame.closure.captured[*captured_index], 0, load(*index))
                        },
                        Instr::Demote(index) => {
                            let cell_value = load(*index);
                            cell_value.ensure_type(TypeId::CELL).unwrap();
                            store(*index, unsafe { values_read(cell_value.addr(), 0) });
                        }
                        Instr::SetPromoted(dst, index) => {
                            let dst_value = load(*dst);
                            dst_value.ensure_type(TypeId::CELL).unwrap();
                            unsafe { values_write(dst_value.addr(), 0, load(*index)) }
                        }

                        Instr::MakeRecordType(dst, layout) => {
                            let type_id = self.registry.add_record_type(layout.clone());
                            store(*dst, Value::new_type_id(type_id));
                        }
                        Instr::MakeRecord(dst, type_id, attr_indexes) => {
                            let type_id = load(*type_id).load_type_id()?;
                            let Some(RecordLayout(layout)) =
                                self.registry.get_record_layout(type_id)
                            else {
                                return Err(ExecuteError::NotRecord);
                            };
                            let record_value =
                                Value::alloc_record(type_id, layout.len(), &mut self.space)?;
                            for &(attr, index) in attr_indexes {
                                let Some(pos) =
                                    layout.iter().position(|&layout_attr| layout_attr == attr)
                                else {
                                    return Err(ExecuteError::UnexpectedAttr(
                                        asset.get_string(attr).into(),
                                    ));
                                };
                                unsafe { values_write(record_value.addr(), pos, load(index)) }
                            }
                            if attr_indexes.len() != layout.len() {
                                return Err(ExecuteError::MissingAttr);
                            }
                            store(*dst, record_value)
                        }
                        Instr::GetAttr(dst, record, attr) => {
                            let record_value = load(*record);
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
                            store(*dst, unsafe { values_read(record_value.addr(), pos) })
                        }
                        Instr::SetAttr(record, attr, index) => {
                            let record_value = load(*record);
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
                            unsafe { values_write(record_value.addr(), pos, load(*index)) }
                        }

                        Instr::MakeUnit(dst) => store(*dst, Value::new_unit()),
                        Instr::MakeString(dst, literal) => {
                            store(*dst, Value::alloc_string(literal, &mut self.space)?)
                        }
                        Instr::MakeI32(dst, value) => store(*dst, Value::new_int32(*value)),

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
                                    let a = load(*a).load_int32()?;
                                    let b = load(*b).load_int32()?;
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
                            store(*dst, dst_value)
                        }

                        Instr::Copy(dst, src) => store(*dst, load(*src)),
                        Instr::Intrinsic(intrinsic, indexes) => {
                            let values = IntrinsicValues {
                                values_addr: task.values_addr,
                                value_offset: frame.value_offset,
                                indexes,
                            };
                            unsafe { intrinsic(values, self, asset) }?
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

unsafe fn values_read(addr: SpaceAddr, pos: usize) -> Value {
    unsafe { addr.cast::<Value>().add(pos).read() }
}

unsafe fn values_write(addr: SpaceAddr, pos: usize, value: Value) {
    unsafe { addr.cast::<Value>().add(pos).write(value) }
}

pub struct IntrinsicValues<'a> {
    values_addr: SpaceAddr,
    value_offset: usize,
    indexes: &'a [usize],
}

impl IntrinsicValues<'_> {
    pub fn load(&self, i: usize) -> Value {
        unsafe { values_read(self.values_addr, self.value_offset + self.indexes[i]) }
    }

    pub fn store(&self, i: usize, value: Value) {
        unsafe { values_write(self.values_addr, self.value_offset + self.indexes[i], value) }
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
    // load_x       (self) -> X?                    access inline type
    // store_x      (self, X) -> ()?
    // load_x       unsafe (self) -> X?             access heap-allocated type with
    // store_x      unsafe (self, X) -> ()?         value semantic
    // get_x        unsafe (self) -> &X?            ... with reference-semantic
    // get_x_mut    unsafe (self) -> &mut X?
    // TODO document here
    // safety: concurrent borrows to identical Value must follow mutability rule
    // actually, probably there should be no concurrent borrow

    // TypeId
    fn new_type_id(TypeId(id): TypeId) -> Self {
        Self::new_inline(TypeId::TYPE_ID, id as _)
    }

    fn load_type_id(self) -> Result<TypeId, TypeError> {
        self.ensure_type(TypeId::TYPE_ID)?;
        Ok(TypeId(self.data() as _))
    }

    // Closure
    fn alloc_closure(closure: Closure, space: &mut Space) -> Result<Self, OutOfSpace> {
        let addr = space.typed_alloc::<Closure>()?;
        unsafe { addr.cast::<Closure>().write(closure) }
        Ok(Self::new(TypeId::CLOSURE, addr))
    }

    unsafe fn load_closure(self) -> Result<Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(unsafe { self.addr().cast::<Closure>().read() })
    }

    unsafe fn get_closure_mut<'a>(self) -> Result<&'a mut Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(unsafe { self.addr().cast::<Closure>().as_mut() })
    }

    // cell type, represented with a single SpaceAddr
    fn alloc_cell(value: Value, space: &mut Space) -> Result<Self, OutOfSpace> {
        let addr = space.typed_alloc::<Value>()?;
        unsafe { addr.cast::<Value>().write(value) }
        Ok(Self::new(TypeId::CELL, addr))
    }
    // access to cell is performed directly with SpaceAddr, not going through Value

    fn alloc_task(space: &mut Space) -> Result<Value, OutOfSpace> {
        let addr = space.typed_alloc::<Task>()?;
        unsafe { addr.cast::<Task>().write(Default::default()) }
        Ok(Value::new(TypeId::TASK, addr))
    }

    // unit type
    fn new_unit() -> Self {
        Self::new_phantom(TypeId::UNIT)
    }

    fn load_unit(self) -> Result<Unit, TypeError> {
        self.ensure_type(TypeId::UNIT)?;
        Ok(Unit)
    }

    // Int32 type
    fn new_int32(value: i32) -> Self {
        let mut data = [0; size_of::<u64>()];
        data[..size_of::<i32>()].copy_from_slice(&value.to_ne_bytes());
        Self::new_inline(TypeId::INT32, u64::from_ne_bytes(data))
    }

    fn load_int32(self) -> Result<i32, TypeError> {
        self.ensure_type(TypeId::INT32)?;
        let mut value = [0; size_of::<i32>()];
        value.copy_from_slice(&self.data().to_ne_bytes()[..size_of::<i32>()]);
        Ok(i32::from_ne_bytes(value))
    }

    // boolean types
    pub fn new_true() -> Self {
        Self::new_phantom(TypeId::TRUE)
    }

    pub fn new_false() -> Self {
        Self::new_phantom(TypeId::FALSE)
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
