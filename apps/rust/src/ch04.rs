//! Chapter 04: Thread Basics

use std::thread::{self, JoinHandle};

/// Create and start a new thread
pub fn create_thread<F>(f: F) -> JoinHandle<()>
where
    F: FnOnce() + Send + 'static,
{
    thread::spawn(f)
}

/// Run multiple threads and wait for completion
pub fn run_threads<F>(count: usize, f: F)
where
    F: Fn(usize) + Send + Sync + Clone + 'static,
{
    let handles: Vec<_> = (0..count)
        .map(|i| {
            let f = f.clone();
            thread::spawn(move || f(i))
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[test]
    fn test_create_thread() {
        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = Arc::clone(&counter);

        let handle = create_thread(move || {
            counter_clone.fetch_add(1, Ordering::SeqCst);
        });

        handle.join().unwrap();
        assert_eq!(counter.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_run_threads() {
        let counter = Arc::new(AtomicUsize::new(0));

        let counter_clone = Arc::clone(&counter);
        run_threads(5, move |_| {
            counter_clone.fetch_add(1, Ordering::SeqCst);
        });

        assert_eq!(counter.load(Ordering::SeqCst), 5);
    }

    #[test]
    fn test_run_threads_with_index() {
        let sum = Arc::new(AtomicUsize::new(0));

        let sum_clone = Arc::clone(&sum);
        run_threads(4, move |i| {
            sum_clone.fetch_add(i, Ordering::SeqCst);
        });

        // 0 + 1 + 2 + 3 = 6
        assert_eq!(sum.load(Ordering::SeqCst), 6);
    }
}
