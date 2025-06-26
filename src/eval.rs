use thiserror::Error;

use crate::{
    code::{Block, Instr, InstrIndex},
    space::{Space, SpaceAddr},
};

#[derive(Debug, Clone, Copy)]
pub struct Value {
    type_id: TypeId,
    addr: SpaceAddr,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeId(u8);

impl TypeId {
    pub const VACANT: Self = Self(0);
    pub const UNIT: Self = Self(1);
    pub const STRING: Self = Self(2);
    pub const CLOSURE: Self = Self(3);
    pub const FUTURE: Self = Self(4);
}

impl Default for Value {
    fn default() -> Self {
        Self {
            type_id: TypeId::VACANT,
            addr: SpaceAddr::MAX,
        }
    }
}

pub struct Unit;

pub struct Closure {
    block: *const Block,
    captured: [Value; 16],
}

impl Closure {
    pub fn new(block: &Block, captured_values: impl ExactSizeIterator<Item = Value>) -> Self {
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

pub struct Eval {
    frames: Vec<Frame>,
}

struct Frame {
    block: *const Block,
    instr_pointer: InstrIndex,
    call_dst: Option<usize>,
    values: Vec<Value>,
}

impl Eval {
    pub fn new(closure: &Closure) -> Result<Self, ArityError> {
        let mut eval = Self {
            frames: Default::default(),
        };
        eval.push_frame(closure, &[])?;
        Ok(eval)
    }
}

#[derive(Error, Debug)]
#[error("arity error: accept {expected} arguments, but got {actual}")]
pub struct ArityError {
    pub expected: usize,
    pub actual: usize,
}

impl Eval {
    fn push_frame(&mut self, closure: &Closure, args: &[Value]) -> Result<(), ArityError> {
        let block = unsafe { &*closure.block };
        if args.len() != block.num_param {
            return Err(ArityError {
                expected: block.num_param,
                actual: args.len(),
            });
        }
        let mut values = vec![Value::default(); block.num_value];
        assert!(values.len() >= block.num_captured + args.len());
        for (dst, src) in values.iter_mut().zip(closure.captured.iter().chain(args)) {
            *dst = *src
        }
        self.frames.push(Frame {
            block,
            instr_pointer: 0,
            call_dst: None,
            values,
        });
        Ok(())
    }
}

pub enum ExecuteStatus {
    Blocking,
    Exited,
}

#[derive(Error, Debug)]
pub enum ExecuteError {
    #[error(transparent)]
    TypeError(#[from] TypeError),
    #[error(transparent)]
    ArityError(#[from] ArityError),
}

impl Eval {
    pub fn execute(&mut self, space: &mut Space) -> Result<ExecuteStatus, ExecuteError> {
        let mut frame = self.frames.last_mut().expect("last frame exists");
        'control: loop {
            let name = unsafe { &(*frame.block).name };
            // optimize for sequentially execute instructions
            for instr in &unsafe { &*frame.block }.instrs[frame.instr_pointer..] {
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
                        let closure = closure_value.get_closure(space)?;
                        self.push_frame(closure, &arg_values)?;
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
                    Instr::Jump(instr_index, None) => {
                        frame.instr_pointer = *instr_index;
                        continue 'control;
                    }
                    Instr::Jump(_, Some(_)) => todo!(),

                    Instr::LoadUnit(dst) => frame.values[*dst] = Value::unit(),
                    Instr::LoadString(_, _) => {}
                    Instr::LoadClosure(dst, block, captured) => {
                        let closure =
                            Closure::new(block, captured.iter().map(|&index| frame.values[index]));
                        let mut closure_value = Value {
                            type_id: TypeId::CLOSURE,
                            addr: space.alloc::<Closure>(),
                        };
                        closure_value.store_closure(closure, space)?;
                        frame.values[*dst] = closure_value
                    }
                    Instr::Copy(dst, src) => {
                        let src_value = frame.values[*src];
                        frame.values[*dst] = src_value
                    }
                    Instr::Spawn(_) => todo!(),
                    Instr::LoadFuture(_) => todo!(),
                    Instr::Wait(_, _) => {
                        // TODO
                        return Ok(ExecuteStatus::Blocking);
                    }
                    Instr::Notify(_) => todo!(),
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

    fn unit() -> Self {
        Self {
            type_id: TypeId::UNIT,
            addr: SpaceAddr::MAX,
        }
    }

    fn load_unit(&self) -> Result<Unit, TypeError> {
        self.ensure_type(TypeId::UNIT)?;
        Ok(Unit)
    }

    fn get_closure<'a>(&'a self, space: &'a Space) -> Result<&'a Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(unsafe { space.get(self.addr) })
    }

    fn store_closure(&mut self, closure: Closure, space: &mut Space) -> Result<(), TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        unsafe { space.write(self.addr, closure) };
        Ok(())
    }
}
