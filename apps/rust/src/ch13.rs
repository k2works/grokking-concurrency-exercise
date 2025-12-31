//! Chapter 13: MapReduce (Word Count)

use rayon::prelude::*;
use std::collections::HashMap;

/// Map: Convert text to (word, 1) pairs
pub fn map(text: &str) -> Vec<(String, usize)> {
    text.to_lowercase()
        .split_whitespace()
        .filter(|s| !s.is_empty())
        .map(|word| (word.to_string(), 1))
        .collect()
}

/// Reduce: Aggregate word counts
pub fn reduce(pairs: Vec<(String, usize)>) -> HashMap<String, usize> {
    let mut result = HashMap::new();
    for (word, count) in pairs {
        *result.entry(word).or_insert(0) += count;
    }
    result
}

/// MapReduce: Count words in multiple texts using parallel execution
pub fn count_words(texts: &[&str]) -> HashMap<String, usize> {
    // Map phase (parallel)
    let mapped: Vec<(String, usize)> = texts
        .par_iter()
        .flat_map(|text| map(text))
        .collect();

    // Reduce phase
    reduce(mapped)
}

/// Parallel reduce using Rayon
pub fn count_words_full_parallel(texts: &[&str]) -> HashMap<String, usize> {
    texts
        .par_iter()
        .flat_map(|text| map(text))
        .fold(
            || HashMap::new(),
            |mut acc, (word, count)| {
                *acc.entry(word).or_insert(0) += count;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map() {
        let result = map("Hello World");
        assert_eq!(result.len(), 2);
        assert!(result.contains(&("hello".to_string(), 1)));
        assert!(result.contains(&("world".to_string(), 1)));
    }

    #[test]
    fn test_map_empty() {
        let result = map("");
        assert!(result.is_empty());
    }

    #[test]
    fn test_reduce() {
        let pairs = vec![
            ("hello".to_string(), 1),
            ("world".to_string(), 1),
            ("hello".to_string(), 1),
        ];
        let result = reduce(pairs);
        assert_eq!(result.get("hello"), Some(&2));
        assert_eq!(result.get("world"), Some(&1));
    }

    #[test]
    fn test_count_words() {
        let texts = vec!["hello world", "hello rust", "world of rust"];
        let result = count_words(&texts);

        assert_eq!(result.get("hello"), Some(&2));
        assert_eq!(result.get("world"), Some(&2));
        assert_eq!(result.get("rust"), Some(&2));
        assert_eq!(result.get("of"), Some(&1));
    }

    #[test]
    fn test_count_words_empty() {
        let texts: Vec<&str> = vec![];
        let result = count_words(&texts);
        assert!(result.is_empty());
    }

    #[test]
    fn test_count_words_full_parallel() {
        let texts = vec!["hello world", "hello rust", "world of rust"];
        let result = count_words_full_parallel(&texts);

        assert_eq!(result.get("hello"), Some(&2));
        assert_eq!(result.get("world"), Some(&2));
        assert_eq!(result.get("rust"), Some(&2));
        assert_eq!(result.get("of"), Some(&1));
    }

    #[test]
    fn test_count_words_large() {
        let text = "the quick brown fox jumps over the lazy dog";
        let texts: Vec<&str> = (0..100).map(|_| text).collect();
        let result = count_words(&texts);

        assert_eq!(result.get("the"), Some(&200)); // "the" appears twice per text
        assert_eq!(result.get("fox"), Some(&100));
    }
}
