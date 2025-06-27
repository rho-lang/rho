use std::collections::HashMap;

use crate::{
    eval::Closure,
    sched::{Sched, SchedStatus},
    space::Space,
    task::{RunStatus, Task},
};

pub fn run(main_closure: &Closure) {
    let mut tasks = HashMap::new();
    let mut space = Space::new(4 << 10);
    let mut sched = Sched::new();

    sched.spawn(Task::new(main_closure).expect("top level task is created with \"main\" closure"));
    loop {
        sched.install_spawned_tasks(&mut tasks);
        let task_id = match sched.advance() {
            SchedStatus::Idle => break,
            SchedStatus::Ready(task_id) => task_id,
            SchedStatus::Blocked => {
                // TODO
                continue;
            }
        };
        let task = tasks.get_mut(&task_id).expect("task {task_id} exists");
        match task.run(&mut space, &mut sched) {
            Ok(RunStatus::Exited) => {
                tasks.remove(&task_id);
            }
            Ok(RunStatus::Waiting(notify_token)) => sched.block(task_id, notify_token),
            Err(err) => {
                tracing::error!(%task_id, %err, "task failed");
                tasks.remove(&task_id);
            }
        }
    }
}
