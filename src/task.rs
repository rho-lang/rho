use crate::{
    eval::{Closure, Eval, ExecuteError, ExecuteStatus},
    sched::Sched,
    space::Space,
};

pub struct Task {
    eval: Eval,
}

pub enum RunStatus {
    Blocking,
    Exited,
}

impl Task {
    pub fn new(closure: &Closure) -> Result<Self, ExecuteError> {
        let eval = Eval::new(closure)?;
        Ok(Self { eval })
    }

    pub fn run(&mut self, space: &mut Space, sched: &mut Sched) -> Result<RunStatus, ExecuteError> {
        let status = match self.eval.execute(space, sched)? {
            ExecuteStatus::Blocking => RunStatus::Blocking,
            ExecuteStatus::Exited => RunStatus::Exited,
        };
        Ok(status)
    }
}
