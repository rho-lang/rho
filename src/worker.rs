use std::{collections::HashMap, mem::replace, sync::Arc};

use crate::{
    asset::Asset,
    eval::{Closure, Eval, ExecuteError},
    sched::{NotifyToken, Sched, SchedStatus},
    space::{OutOfSpace, Space},
    task::{RunStatus, Task},
    typing::TypeRegistry,
};

pub struct WorkerContext {
    pub registry: TypeRegistry,
    pub space: Space,
    pub sched: Sched,
    pub oracle_advance: NotifyToken,
}

pub fn run(main_closure: Closure, registry: TypeRegistry, asset: Arc<Asset>) {
    let mut tasks = HashMap::new();
    let mut sched = Sched::new();
    let mut context = WorkerContext {
        registry,
        space: Space::new(4 << 10),
        oracle_advance: sched.alloc_notify_token(),
        sched,
    };

    let mut eval = Eval::new();
    if let Err(err) = eval.init(main_closure, &asset) {
        tracing::error!(%err, "top level task is not \"main-complaint\"");
        return;
    };
    context.sched.spawn(Task::new(eval));

    let mut scheduling_oracle = false;
    loop {
        context.sched.install_spawned_tasks(&mut tasks);
        let task_id = match context.sched.advance() {
            SchedStatus::Idle => break,
            SchedStatus::Ready(task_id) => task_id,
            SchedStatus::Halted if !scheduling_oracle => {
                tracing::trace!("schedule oracle advance");
                context.sched.notify_clear(context.oracle_advance);
                scheduling_oracle = true;
                continue;
            }
            SchedStatus::Halted => {
                tracing::error!("worker halted");
                break;
            }
        };
        tracing::trace!(task_id, "schedule");
        scheduling_oracle = false;
        let mut task_ready = true;
        while replace(&mut task_ready, false) {
            let task = tasks.get_mut(&task_id).expect("task {task_id} exists");
            match task.run(&mut context, &asset) {
                Ok(RunStatus::Waiting(notify_token)) => context.sched.wait(task_id, notify_token),
                Ok(RunStatus::Exited) => {
                    tasks.remove(&task_id);
                }
                Err(ExecuteError::Space(out_of_space @ OutOfSpace(_))) => {
                    context.space.copy_collect(out_of_space, tasks.values_mut());
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
