//! Chapter 05: Password Cracker (Parallel)

use rayon::prelude::*;
use crate::ch02::get_crypto_hash;

/// Crack password using parallel execution with Rayon
pub fn crack_password_parallel(
    crypto_hash: &str,
    alphabet: &[char],
    length: usize,
) -> Option<String> {
    if length == 0 {
        return None;
    }

    // Generate all combinations of first character
    let first_chars: Vec<char> = alphabet.to_vec();

    first_chars
        .par_iter()
        .find_map_any(|&first| {
            crack_recursive(crypto_hash, alphabet, first.to_string(), length - 1)
        })
}

fn crack_recursive(
    crypto_hash: &str,
    alphabet: &[char],
    prefix: String,
    remaining: usize,
) -> Option<String> {
    if remaining == 0 {
        if get_crypto_hash(&prefix) == crypto_hash {
            return Some(prefix);
        }
        return None;
    }

    for &c in alphabet {
        let mut candidate = prefix.clone();
        candidate.push(c);
        if let Some(result) = crack_recursive(crypto_hash, alphabet, candidate, remaining - 1) {
            return Some(result);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crack_password_parallel_found() {
        let alphabet: Vec<char> = "ab".chars().collect();
        let target_hash = get_crypto_hash("ba");
        let result = crack_password_parallel(&target_hash, &alphabet, 2);
        assert_eq!(result, Some("ba".to_string()));
    }

    #[test]
    fn test_crack_password_parallel_not_found() {
        let alphabet: Vec<char> = "ab".chars().collect();
        let target_hash = get_crypto_hash("xyz");
        let result = crack_password_parallel(&target_hash, &alphabet, 2);
        assert_eq!(result, None);
    }

    #[test]
    fn test_crack_password_parallel_length_zero() {
        let alphabet: Vec<char> = "ab".chars().collect();
        let target_hash = get_crypto_hash("ab");
        let result = crack_password_parallel(&target_hash, &alphabet, 0);
        assert_eq!(result, None);
    }

    #[test]
    fn test_crack_password_parallel_longer() {
        let alphabet: Vec<char> = "abc".chars().collect();
        let target_hash = get_crypto_hash("cab");
        let result = crack_password_parallel(&target_hash, &alphabet, 3);
        assert_eq!(result, Some("cab".to_string()));
    }
}
