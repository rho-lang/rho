use std::{slice, str};

use thiserror::Error;

use crate::{
    code::{Block, Instr, InstrIndex},
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
    block: *const Block,
    captured: [Value; 16],
}

impl Closure {
    /// # Safety
    /// `block` must outlive Self.
    pub unsafe fn new(
        block: &Block,
        captured_values: impl ExactSizeIterator<Item = Value>,
    ) -> Self {
        assert!(
            block.num_captured <= 16,
            "capturing more than 16 values is not supported"
        );
        assert_eq!(captured_values.len(), block.num_captured);
        let mut captured = [Value::default(); 16];
        for (dst, src) in captured.iter_mut().zip(captured_values) {
            *dst = src
        }
        Self { block, captured }
    }

    pub fn main(instrs: Vec<Instr>, num_value: usize) -> Self {
        let block = Box::new(Block {
            name: "<main>".into(),
            instrs,
            num_param: 0,
            num_captured: 0,
            num_value,
        });
        Self {
            block: Box::into_raw(block),
            captured: Default::default(),
        }
    }

    pub unsafe fn drop_main(self) {
        // SAFETY: `block` is created by `Box::into_raw`, so it must be valid.
        drop(unsafe { Box::from_raw(self.block as *mut Block) });
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

    pub fn init(&mut self, closure: Closure) -> Result<(), ArityError> {
        self.push_frame(closure, &[])
    }
}

#[derive(Error, Debug)]
#[error("arity error: accept {expected} arguments, but got {actual}")]
pub struct ArityError {
    pub expected: usize,
    pub actual: usize,
}

impl Eval {
    fn push_frame(&mut self, closure: Closure, args: &[Value]) -> Result<(), ArityError> {
        let block = unsafe { &*closure.block };
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
    ) -> Result<ExecuteStatus, ExecuteError> {
        // the contract here is `execute` should only be called after calling `init`,
        // and should not be called again after returning Exited
        let mut frame = self.frames.last_mut().expect("last frame exists");
        'control: loop {
            let name = unsafe { &(*frame.closure.block).name };
            // optimize for sequentially execute instructions
            for instr in &unsafe { &*frame.closure.block }.instrs[frame.instr_pointer..] {
                tracing::trace!(%frame.instr_pointer, %name, ?instr, "execute");
                frame.instr_pointer += 1;
                match instr {
                    Instr::Call(dst, closure, args) => {
                        let closure_value = frame.values[*closure];
                        let arg_values = args
                            .iter()
                            .map(|arg| frame.values[*arg])
                            .collect::<Vec<_>>();
                        let replaced = frame.call_dst.replace(*dst);
                        assert!(replaced.is_none());
                        // borrow to `self` via `frame` ends here
                        self.push_frame(closure_value.load_closure(space)?, &arg_values)?;
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

                    Instr::MakeUnit(dst) => frame.values[*dst] = Value::unit(),
                    Instr::MakeString(dst, literal) => {
                        frame.values[*dst] = Value::alloc_string(literal, space)
                    }
                    Instr::MakeClosure(dst, block, captured) => {
                        let closure = unsafe {
                            Closure::new(block, captured.iter().map(|&index| frame.values[index]))
                        };
                        let mut closure_value = Value {
                            type_id: TypeId::CLOSURE,
                            data: space.typed_alloc::<Closure>(),
                        };
                        closure_value.store_closure(closure, space)?;
                        frame.values[*dst] = closure_value
                    }
                    Instr::MakeFuture(dst) => {
                        let future = Future(sched.alloc_notify_token());
                        frame.values[*dst] = Value::alloc_future(future, space)
                    }
                    Instr::MakeRecordType(dst, layout) => {
                        let type_id = registry.add_record_type(layout.clone());
                        frame.values[*dst] = Value::type_id(type_id)
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
                                return Err(ExecuteError::UnexpectedAttr("".into())); // TODO
                            };
                            record_attrs[pos] = frame.values[*index]
                        }
                        if attrs.len() != layout.len() {
                            return Err(ExecuteError::MissingAttr);
                        }
                        frame.values[*dst] = Value::alloc_record(type_id, record_attrs, space)
                    }

                    Instr::Copy(dst, src) => {
                        let src_value = frame.values[*src];
                        frame.values[*dst] = src_value
                    }
                    Instr::CopyCaptured(dst, index) => {
                        assert!(*index < unsafe { &*frame.closure.block }.num_captured);
                        frame.values[*dst] = frame.closure.captured[*index]
                    }

                    Instr::Spawn(closure) => {
                        let mut eval = Self::new();
                        eval.init(frame.values[*closure].load_closure(space)?)?;
                        sched.spawn(Task::new(eval));
                    }
                    Instr::Wait(future) => {
                        let Future(notify_token) = frame.values[*future].load_future(space)?;
                        return Ok(ExecuteStatus::Waiting(notify_token));
                    }
                    Instr::Notify(future) => {
                        let Future(notify_token) = frame.values[*future].load_future(space)?;
                        sched.notify(notify_token)
                    }
                }
            }
            unreachable!("block should terminate with Return")
        }
    }
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

    fn type_id(type_id: TypeId) -> Self {
        Self {
            type_id: TypeId::TYPE_ID,
            data: type_id.0 as _,
        }
    }

    fn load_type_id(&self) -> Result<TypeId, TypeError> {
        self.ensure_type(TypeId::TYPE_ID)?;
        Ok(TypeId(self.load_inline() as _))
    }

    fn alloc_record(type_id: TypeId, attrs: Vec<Value>, space: &mut Space) -> Self {
        let addr = space.alloc(size_of::<Value>() * attrs.len(), align_of::<Value>());
        let buf = space
            .get_mut(addr, size_of::<Value>() * attrs.len())
            .as_mut_ptr()
            .cast::<Value>();
        let record_attrs = unsafe { slice::from_raw_parts_mut(buf, attrs.len()) };
        for (dst, src) in record_attrs.into_iter().zip(attrs) {
            *dst = src
        }
        Self {
            type_id,
            data: addr,
        }
    }

    fn get_record<'a>(&'a self, num_attr: usize, space: &'a Space) -> &'a [Value] {
        let buf = space
            .get(self.data, size_of::<Value>() * num_attr)
            .as_ptr()
            .cast::<Value>();
        assert!(buf.is_aligned());
        unsafe { slice::from_raw_parts(buf, num_attr) }
    }

    fn unit() -> Self {
        Self {
            type_id: TypeId::UNIT,
            data: SpaceAddr::MAX,
        }
    }

    fn load_unit(&self) -> Result<Unit, TypeError> {
        self.ensure_type(TypeId::UNIT)?;
        Ok(Unit)
    }

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

    fn get_str<'a>(&'a self, space: &'a Space) -> Result<&'a str, TypeError> {
        self.ensure_type(TypeId::STRING)?;
        let string = unsafe { space.typed_get::<String>(self.data) };
        Ok(unsafe { str::from_utf8_unchecked(space.get(string.buf, string.len)) })
    }

    fn load_closure(&self, space: &Space) -> Result<Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(*unsafe { space.typed_get(self.data) })
    }

    fn store_closure(&mut self, closure: Closure, space: &mut Space) -> Result<(), TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        unsafe { space.typed_write(self.data, closure) };
        Ok(())
    }

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
        let message = values[indexes[0]].get_str(space)?;
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
            .get_str(space)?
            .parse::<humantime::Duration>()
            .map_err(Error::ParseDuration)?
            .into();
        oracle.notify_after(notify_token, duration)?;
        Ok(())
    }
}
