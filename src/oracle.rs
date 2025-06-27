use std::{
    collections::{BTreeMap, HashMap},
    thread::sleep,
    time::{Duration, Instant},
};

use thiserror::Error;

use crate::sched::{NotifyToken, Sched};

#[derive(Debug)]
pub enum Oracle {
    TimerQueue(TimerQueue),
}

impl Oracle {
    pub fn notify_after(
        &mut self,
        notify_token: NotifyToken,
        duration: Duration,
    ) -> Result<(), TimerError> {
        match self {
            Self::TimerQueue(queue) => queue.notify_after(notify_token, duration),
        }
    }

    pub fn cancel(&mut self, notify_token: NotifyToken) -> Result<(), TimerError> {
        match self {
            Self::TimerQueue(queue) => queue.cancel(notify_token),
        }
    }

    pub fn advance(&mut self, sched: &mut Sched) -> bool {
        match self {
            Self::TimerQueue(queue) => queue.advance(sched),
        }
    }
}

#[derive(Debug, Default)]
pub struct TimerQueue {
    notify_tokens: BTreeMap<(Instant, TimerId), NotifyToken>,
    notify_token_index: HashMap<NotifyToken, (Instant, TimerId)>,
    timer_id: TimerId,
}

type TimerId = u64;

impl TimerQueue {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Error)]
pub enum TimerError {
    #[error("double insertion of notify token {0}")]
    DoubleInsert(NotifyToken),
    #[error("canceled notify token {0} not exist")]
    CancelNotExist(NotifyToken),
}

impl TimerQueue {
    pub fn notify_after(
        &mut self,
        notify_token: NotifyToken,
        duration: Duration,
    ) -> Result<(), TimerError> {
        self.timer_id += 1;
        let timer_id = self.timer_id;
        let instant = Instant::now() + duration;
        let replaced = self
            .notify_token_index
            .insert(notify_token, (instant, timer_id));
        if replaced.is_some() {
            return Err(TimerError::DoubleInsert(notify_token));
        }
        self.notify_tokens.insert((instant, timer_id), notify_token);
        Ok(())
    }

    pub fn cancel(&mut self, notify_token: NotifyToken) -> Result<(), TimerError> {
        let Some(key) = self.notify_token_index.remove(&notify_token) else {
            return Err(TimerError::CancelNotExist(notify_token));
        };
        self.notify_tokens
            .remove(&key)
            .expect("notify token in the queue");
        Ok(())
    }

    pub fn advance(&mut self, sched: &mut Sched) -> bool {
        let Some(((instant, _), notify_token)) = self.notify_tokens.pop_first() else {
            return false;
        };
        self.notify_token_index.remove(&notify_token);
        if let Some(duration) = instant.checked_duration_since(Instant::now()) {
            sleep(duration)
        }
        sched.notify(notify_token);
        true
    }
}
