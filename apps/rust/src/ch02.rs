//! Chapter 02: Password Cracker (Sequential)

use sha2::{Sha256, Digest};

/// Compute SHA-256 hash of a password
pub fn get_crypto_hash(password: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(password.as_bytes());
    let result = hasher.finalize();
    format!("{:x}", result)
}

/// Crack password by brute force (sequential)
pub fn crack_password(crypto_hash: &str, alphabet: &[char], length: usize) -> Option<String> {
    if length == 0 {
        return None;
    }
    crack_recursive(crypto_hash, alphabet, String::new(), length)
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
    fn test_get_crypto_hash() {
        let hash = get_crypto_hash("ab");
        assert_eq!(
            hash,
            "fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"
        );
    }

    #[test]
    fn test_crack_password_found() {
        let alphabet: Vec<char> = "ab".chars().collect();
        let target_hash = get_crypto_hash("ab");
        let result = crack_password(&target_hash, &alphabet, 2);
        assert_eq!(result, Some("ab".to_string()));
    }

    #[test]
    fn test_crack_password_not_found() {
        let alphabet: Vec<char> = "ab".chars().collect();
        let target_hash = get_crypto_hash("xyz");
        let result = crack_password(&target_hash, &alphabet, 2);
        assert_eq!(result, None);
    }

    #[test]
    fn test_crack_password_length_zero() {
        let alphabet: Vec<char> = "ab".chars().collect();
        let target_hash = get_crypto_hash("ab");
        let result = crack_password(&target_hash, &alphabet, 0);
        assert_eq!(result, None);
    }
}
