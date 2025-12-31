//! Chapter 06: Game Loop (Multitasking)

use std::sync::{Arc, Condvar, Mutex};
use std::thread;

/// A game task that can be processed
pub struct GameTask {
    pub name: String,
    pub action: Box<dyn Fn() + Send + Sync>,
}

impl GameTask {
    pub fn new<F>(name: &str, action: F) -> Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        GameTask {
            name: name.to_string(),
            action: Box::new(action),
        }
    }

    pub fn execute(&self) {
        (self.action)();
    }
}

/// Event for signaling processor availability
pub struct ProcessorFreeEvent {
    state: Mutex<bool>,
    condvar: Condvar,
}

impl ProcessorFreeEvent {
    pub fn new() -> Self {
        ProcessorFreeEvent {
            state: Mutex::new(false),
            condvar: Condvar::new(),
        }
    }

    pub fn wait(&self) {
        let mut ready = self.state.lock().unwrap();
        while !*ready {
            ready = self.condvar.wait(ready).unwrap();
        }
        *ready = false;
    }

    pub fn signal(&self) {
        let mut ready = self.state.lock().unwrap();
        *ready = true;
        self.condvar.notify_one();
    }
}

impl Default for ProcessorFreeEvent {
    fn default() -> Self {
        Self::new()
    }
}

/// Simple game loop that processes tasks
pub fn game_loop(tasks: Vec<GameTask>, iterations: usize) {
    for _ in 0..iterations {
        for task in &tasks {
            task.execute();
        }
    }
}

/// Cooperative game loop with event signaling
pub fn cooperative_game_loop(
    tasks: Vec<Arc<GameTask>>,
    event: Arc<ProcessorFreeEvent>,
    iterations: usize,
) {
    for _ in 0..iterations {
        for task in &tasks {
            task.execute();
            event.signal();
            thread::yield_now();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_game_task_execute() {
        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = Arc::clone(&counter);

        let task = GameTask::new("test", move || {
            counter_clone.fetch_add(1, Ordering::SeqCst);
        });

        task.execute();
        assert_eq!(counter.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_processor_free_event() {
        let event = Arc::new(ProcessorFreeEvent::new());
        let event_clone = Arc::clone(&event);

        let handle = thread::spawn(move || {
            thread::sleep(std::time::Duration::from_millis(10));
            event_clone.signal();
        });

        event.wait();
        handle.join().unwrap();
    }

    #[test]
    fn test_game_loop() {
        let counter = Arc::new(AtomicUsize::new(0));

        let counter1 = Arc::clone(&counter);
        let counter2 = Arc::clone(&counter);

        let tasks = vec![
            GameTask::new("task1", move || {
                counter1.fetch_add(1, Ordering::SeqCst);
            }),
            GameTask::new("task2", move || {
                counter2.fetch_add(10, Ordering::SeqCst);
            }),
        ];

        game_loop(tasks, 3);
        // 3 iterations * (1 + 10) = 33
        assert_eq!(counter.load(Ordering::SeqCst), 33);
    }

    #[test]
    fn test_cooperative_game_loop() {
        let counter = Arc::new(AtomicUsize::new(0));
        let event = Arc::new(ProcessorFreeEvent::new());

        let counter1 = Arc::clone(&counter);
        let tasks = vec![Arc::new(GameTask::new("task1", move || {
            counter1.fetch_add(1, Ordering::SeqCst);
        }))];

        cooperative_game_loop(tasks, event, 5);
        assert_eq!(counter.load(Ordering::SeqCst), 5);
    }
}
