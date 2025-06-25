use std::collections::HashMap;

use crate::{
    sched::Sched,
    task::{RunStatus, Task},
};

pub fn run(task: Task) {
    let mut tasks = HashMap::new();
    let mut sched = Sched::new();

    sched.spawn(task);
    sched.install_spawned_tasks(&mut tasks);
    while let Some(task_id) = sched.retrieve_next_task() {
        let task = tasks.get_mut(&task_id).expect("task {task_id} exists");
        if let RunStatus::Exited = task.run(&mut sched) {
            tasks.remove(&task_id);
        }
        sched.install_spawned_tasks(&mut tasks);
    }
}
