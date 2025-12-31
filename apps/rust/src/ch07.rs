//! Chapter 07: Fork/Join and Pipeline

use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;

/// Vote Counter using Fork/Join pattern
pub fn count_votes(votes: &[&str]) -> HashMap<String, usize> {
    if votes.is_empty() {
        return HashMap::new();
    }

    // Use parallel fold and reduce
    votes
        .par_iter()
        .fold(
            || HashMap::new(),
            |mut acc: HashMap<String, usize>, &vote| {
                *acc.entry(vote.to_string()).or_insert(0) += 1;
                acc
            },
        )
        .reduce(
            || HashMap::new(),
            |mut a, b| {
                for (k, v) in b {
                    *a.entry(k).or_insert(0) += v;
                }
                a
            },
        )
}

/// A stage in the pipeline
pub struct Stage<I, O>
where
    I: Send + 'static,
    O: Send + 'static,
{
    pub name: String,
    pub processor: Box<dyn Fn(I) -> O + Send>,
}

impl<I, O> Stage<I, O>
where
    I: Send + 'static,
    O: Send + 'static,
{
    pub fn new<F>(name: &str, processor: F) -> Self
    where
        F: Fn(I) -> O + Send + 'static,
    {
        Stage {
            name: name.to_string(),
            processor: Box::new(processor),
        }
    }

    pub fn process(&self, input: I) -> O {
        (self.processor)(input)
    }
}

/// Pipeline that processes items through multiple stages
pub struct Pipeline {
    stages: Vec<Box<dyn Fn(i32) -> i32 + Send>>,
}

impl Default for Pipeline {
    fn default() -> Self {
        Self::new()
    }
}

impl Pipeline {
    pub fn new() -> Self {
        Pipeline { stages: Vec::new() }
    }

    pub fn add_stage<F>(mut self, processor: F) -> Self
    where
        F: Fn(i32) -> i32 + Send + 'static,
    {
        self.stages.push(Box::new(processor));
        self
    }

    pub fn process(&self, input: i32) -> i32 {
        self.stages.iter().fold(input, |acc, stage| stage(acc))
    }

    pub fn process_all(&self, inputs: Vec<i32>) -> Vec<i32> {
        inputs.into_iter().map(|x| self.process(x)).collect()
    }
}

/// Concurrent pipeline using channels
pub fn concurrent_pipeline<T>(
    input: Vec<T>,
    processors: Vec<Box<dyn Fn(T) -> T + Send + 'static>>,
) -> Vec<T>
where
    T: Send + 'static + Clone,
{
    if processors.is_empty() {
        return input;
    }

    // Create channels between stages
    let (first_tx, mut current_rx): (Sender<T>, Receiver<T>) = channel();

    // Send initial input
    let input_thread = thread::spawn(move || {
        for item in input {
            first_tx.send(item).unwrap();
        }
    });

    // Create processor threads
    let mut handles = vec![input_thread];

    for processor in processors {
        let (tx, rx): (Sender<T>, Receiver<T>) = channel();
        let prev_rx = current_rx;
        current_rx = rx;

        let handle = thread::spawn(move || {
            while let Ok(item) = prev_rx.recv() {
                let result = processor(item);
                if tx.send(result).is_err() {
                    break;
                }
            }
        });
        handles.push(handle);
    }

    // Collect results
    let mut results = Vec::new();
    while let Ok(item) = current_rx.recv() {
        results.push(item);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_votes_empty() {
        let votes: Vec<&str> = vec![];
        let result = count_votes(&votes);
        assert!(result.is_empty());
    }

    #[test]
    fn test_count_votes() {
        let votes = vec!["A", "B", "A", "A", "B", "C"];
        let result = count_votes(&votes);
        assert_eq!(result.get("A"), Some(&3));
        assert_eq!(result.get("B"), Some(&2));
        assert_eq!(result.get("C"), Some(&1));
    }

    #[test]
    fn test_stage() {
        let stage = Stage::new("double", |x: i32| x * 2);
        assert_eq!(stage.process(5), 10);
    }

    #[test]
    fn test_pipeline() {
        let pipeline = Pipeline::new()
            .add_stage(|x| x + 1)
            .add_stage(|x| x * 2)
            .add_stage(|x| x - 3);

        // (5 + 1) * 2 - 3 = 9
        assert_eq!(pipeline.process(5), 9);
    }

    #[test]
    fn test_pipeline_process_all() {
        let pipeline = Pipeline::new()
            .add_stage(|x| x * 2);

        let results = pipeline.process_all(vec![1, 2, 3, 4, 5]);
        assert_eq!(results, vec![2, 4, 6, 8, 10]);
    }

    #[test]
    fn test_concurrent_pipeline() {
        let input = vec![1, 2, 3, 4, 5];
        let processors: Vec<Box<dyn Fn(i32) -> i32 + Send>> = vec![
            Box::new(|x| x + 1),
            Box::new(|x| x * 2),
        ];

        let results = concurrent_pipeline(input, processors);
        // (x + 1) * 2: [4, 6, 8, 10, 12]
        assert_eq!(results, vec![4, 6, 8, 10, 12]);
    }
}
