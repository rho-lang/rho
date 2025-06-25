use crate::sched::Sched;

pub struct Task {
    //
}

pub enum RunStatus {
    Blocking,
    Exited,
}

impl Task {
    pub fn run(&mut self, sched: &mut Sched) -> RunStatus {
        RunStatus::Exited
    }
}
