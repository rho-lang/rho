use std::sync::Arc;

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
pub struct TypeId(u16);

impl TypeId {
    const STRING: Self = Self(1);
    const CLOSURE: Self = Self(2);
    const FUTURE: Self = Self(3);
}

#[derive(Error, Debug)]
#[error("type error: expected {expected:?}, actual {actual:?}")]
pub struct TypeError {
    pub expected: TypeId,
    pub actual: TypeId,
}

pub struct Closure {
    block: Arc<Block>,
    captured: Vec<Value>,
}

pub struct Eval {
    frames: Vec<Frame>,
}

struct Frame {
    block: Arc<Block>,
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
        if args.len() != closure.block.num_param {
            return Err(ArityError {
                expected: closure.block.num_param,
                actual: args.len(),
            });
        }
        self.frames.push(Frame {
            block: closure.block.clone(),
            instr_pointer: 0,
            call_dst: None,
            values: [&closure.captured, args].concat(),
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
        loop {
            let frame = self.frames.last_mut().expect("last frame exists");
            for instr in &frame.block.instrs[frame.instr_pointer..] {
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
                        // borrow to `self` ends here
                        let closure = closure_value.get_closure(space)?;
                        self.push_frame(closure, &arg_values)?;
                        break;
                    }
                    Instr::Return(index) => {
                        let return_value = frame.values[*index];
                        // borrow to `self` ends here
                        self.frames.pop();
                        let Some(frame) = self.frames.last_mut() else {
                            // TODO assert returning Unit
                            return Ok(ExecuteStatus::Exited);
                        };
                        let dst = frame.call_dst.take().expect("call destination must be set");
                        frame.values[dst] = return_value;
                        break;
                    }
                    Instr::Jump(_, jump_cond) => todo!(),
                    Instr::LoadString(_, _) => {}
                    Instr::LoadClosure(dst, block, items) => {
                        let block = block.clone();
                        let captured = items.iter().map(|item| frame.values[*item]).collect();
                        let mut closure_value = Value {
                            type_id: TypeId::CLOSURE,
                            addr: space.alloc::<Closure>(),
                        };
                        closure_value.write_closure(Closure { block, captured }, space)?;
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

    fn get_closure<'a>(&'a self, space: &'a Space) -> Result<&'a Closure, TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        Ok(unsafe { space.get(self.addr) })
    }

    fn write_closure(&mut self, closure: Closure, space: &mut Space) -> Result<(), TypeError> {
        self.ensure_type(TypeId::CLOSURE)?;
        unsafe { space.write(self.addr, closure) };
        Ok(())
    }
}
