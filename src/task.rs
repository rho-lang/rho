use crate::{
    asset::Asset,
    eval::{Eval, ExecuteError},
    worker::WorkerContext,
};

// currently Task is a redundant concept: a Task is no more than a Eval
// tentatively save it for future extension: anything worker-oriented instead
// of language-oriented should go here. and maybe some optimization can make use
// of it

pub struct Task {
    eval: Eval,
}

impl Task {
    pub fn new(eval: Eval) -> Self {
        Self { eval }
    }
}

pub type RunStatus = crate::eval::ExecuteStatus;

impl Task {
    pub fn run(
        &mut self,
        context: &mut WorkerContext,
        asset: &Asset,
    ) -> Result<RunStatus, ExecuteError> {
        self.eval.execute(context, asset)
    }
}
