use std::collections::HashMap;

use crate::{
    eval::Closure,
    sched::Sched,
    space::Space,
    task::{RunStatus, Task},
};

pub fn run(main_closure: &Closure) {
    let mut tasks = HashMap::new();
    let mut space = Space::new(4 << 10);
    let mut sched = Sched::new();

    sched.spawn(Task::new(main_closure).expect("top level task is created with \"main\" closure"));
    sched.install_spawned_tasks(&mut tasks);
    while let Some(task_id) = sched.retrieve_next_task() {
        let task = tasks.get_mut(&task_id).expect("task {task_id} exists");
        match task.run(&mut space, &mut sched) {
            Ok(RunStatus::Exited) => {
                tasks.remove(&task_id);
            }
            Ok(RunStatus::Blocking) => {}
            Err(err) => {
                tracing::error!("task {task_id} error: {err}");
                tasks.remove(&task_id);
            }
        }
        sched.install_spawned_tasks(&mut tasks);
    }
}
