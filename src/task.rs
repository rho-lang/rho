use crate::{
    eval::{Eval, ExecuteError, ExecuteStatus},
    sched::{NotifyToken, Sched},
    space::Space,
};

pub struct Task {
    eval: Eval,
}

pub enum RunStatus {
    Waiting(NotifyToken),
    Exited,
}

impl Task {
    pub fn new(eval: Eval) -> Self {
        Self { eval }
    }

    pub fn run(&mut self, space: &mut Space, sched: &mut Sched) -> Result<RunStatus, ExecuteError> {
        let status = match self.eval.execute(space, sched)? {
            ExecuteStatus::Waiting(notify_token) => RunStatus::Waiting(notify_token),
            ExecuteStatus::Exited => RunStatus::Exited,
        };
        Ok(status)
    }
}
