use std::collections::{HashMap, VecDeque};

use crate::task::Task;

pub type TaskId = u64;
pub type NotifyToken = u64;

pub struct Sched {
    next_task_id: TaskId,
    pending_tasks: Vec<(TaskId, Task)>,
    ready_queue: VecDeque<TaskId>,
    next_notify_token: NotifyToken,
    notify_tasks: HashMap<NotifyToken, Vec<TaskId>>,
}

impl Default for Sched {
    fn default() -> Self {
        Self::new()
    }
}

impl Sched {
    pub fn new() -> Self {
        Self {
            next_task_id: 0,
            pending_tasks: Default::default(),
            ready_queue: Default::default(),
            next_notify_token: 0,
            notify_tasks: Default::default(),
        }
    }

    pub fn spawn(&mut self, task: Task) -> TaskId {
        self.next_task_id += 1;
        let task_id = self.next_task_id;
        self.pending_tasks.push((task_id, task));
        task_id
    }

    pub fn install_spawned_tasks(&mut self, tasks: &mut HashMap<TaskId, Task>) {
        for (task_id, task) in self.pending_tasks.drain(..) {
            let replaced = tasks.insert(task_id, task);
            assert!(replaced.is_none(), "task {task_id} already exists");
            self.ready_queue.push_back(task_id)
        }
    }
}

pub enum SchedStatus {
    Idle,
    Ready(TaskId),
    Blocked,
}

impl Sched {
    pub fn advance(&mut self) -> SchedStatus {
        assert!(
            self.pending_tasks.is_empty(),
            "cannot advance with task(s) pending install"
        );
        if let Some(task_id) = self.ready_queue.pop_front() {
            SchedStatus::Ready(task_id)
        } else if self.notify_tasks.is_empty() {
            SchedStatus::Idle
        } else {
            SchedStatus::Blocked
        }
    }

    pub fn alloc_notify_token(&mut self) -> NotifyToken {
        self.next_notify_token += 1;
        self.next_notify_token
    }

    pub fn block(&mut self, task_id: TaskId, token: NotifyToken) {
        self.notify_tasks.entry(token).or_default().push(task_id)
    }

    pub fn notify(&mut self, token: NotifyToken) {
        if let Some(task_ids) = self.notify_tasks.remove(&token) {
            for task_id in task_ids {
                self.ready_queue.push_back(task_id)
            }
        }
    }

    // TODO figure out a way to identify NotifyToken without potential notifier, if
    // possible
}
