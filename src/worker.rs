use std::{collections::HashMap, mem::replace};

use crate::{
    asset::Asset,
    eval::{Closure, Eval, ExecuteError},
    oracle::Oracle,
    sched::{Sched, SchedStatus},
    space::{OutOfSpace, Space},
    task::{RunStatus, Task},
    typing::TypeRegistry,
};

pub fn run(main_closure: Closure, mut registry: TypeRegistry, asset: &Asset) {
    let mut tasks = HashMap::new();
    let mut space = Space::new(4 << 10);
    let mut sched = Sched::new();
    let mut oracle = Oracle::TimerQueue(Default::default());

    let mut eval = Eval::new();
    if let Err(err) = eval.init(main_closure, asset) {
        tracing::error!(%err, "top level task is not \"main-complaint\"");
        return;
    };
    sched.spawn(Task::new(eval));

    loop {
        sched.install_spawned_tasks(&mut tasks);
        let task_id = match sched.advance() {
            SchedStatus::Idle => break,
            SchedStatus::Ready(task_id) => task_id,
            SchedStatus::Blocked => {
                if oracle.advance(&mut sched) {
                    continue;
                } else {
                    tracing::error!("worker halted");
                    break;
                }
            }
        };
        let mut task_ready = true;
        while replace(&mut task_ready, false) {
            let task = tasks.get_mut(&task_id).expect("task {task_id} exists");
            match task.run(&mut space, &mut registry, &mut sched, &mut oracle, asset) {
                Ok(RunStatus::Waiting(notify_token)) => sched.block(task_id, notify_token),
                Ok(RunStatus::Exited) => {
                    tasks.remove(&task_id);
                }
                Err(ExecuteError::Space(out_of_space @ OutOfSpace(_))) => {
                    space.copy_collect(out_of_space, tasks.values_mut());
                    task_ready = true
                }
                Err(err) => {
                    tracing::error!(%task_id, %err, "task failed");
                    tasks.remove(&task_id);
                }
            }
        }
    }
}
