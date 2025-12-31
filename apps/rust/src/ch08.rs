//! Chapter 08: Bank Account (Synchronization and Mutual Exclusion)

use std::sync::{Arc, Mutex, MutexGuard};

/// A thread-safe bank account
#[derive(Debug)]
pub struct BankAccount {
    balance: Mutex<i64>,
    id: usize,
}

impl BankAccount {
    pub fn new(initial_balance: i64) -> Self {
        static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        BankAccount {
            balance: Mutex::new(initial_balance),
            id: COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        }
    }

    pub fn get_balance(&self) -> i64 {
        *self.balance.lock().unwrap()
    }

    pub fn deposit(&self, amount: i64) {
        let mut balance = self.balance.lock().unwrap();
        *balance += amount;
    }

    pub fn withdraw(&self, amount: i64) -> bool {
        let mut balance = self.balance.lock().unwrap();
        if *balance >= amount {
            *balance -= amount;
            true
        } else {
            false
        }
    }

    /// Get the account ID for ordering locks
    fn id(&self) -> usize {
        self.id
    }
}

/// Transfer money between accounts atomically, avoiding deadlock
pub fn transfer(from: &Arc<BankAccount>, to: &Arc<BankAccount>, amount: i64) -> bool {
    // Always lock in consistent order to avoid deadlock
    let (first, second, from_is_first) = if from.id() < to.id() {
        (from, to, true)
    } else {
        (to, from, false)
    };

    let mut first_guard = first.balance.lock().unwrap();
    let mut second_guard = second.balance.lock().unwrap();

    let (from_guard, to_guard): (&mut MutexGuard<i64>, &mut MutexGuard<i64>) = if from_is_first {
        (&mut first_guard, &mut second_guard)
    } else {
        (&mut second_guard, &mut first_guard)
    };

    if **from_guard >= amount {
        **from_guard -= amount;
        **to_guard += amount;
        true
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_bank_account_new() {
        let account = BankAccount::new(100);
        assert_eq!(account.get_balance(), 100);
    }

    #[test]
    fn test_bank_account_deposit() {
        let account = BankAccount::new(100);
        account.deposit(50);
        assert_eq!(account.get_balance(), 150);
    }

    #[test]
    fn test_bank_account_withdraw_success() {
        let account = BankAccount::new(100);
        let result = account.withdraw(30);
        assert!(result);
        assert_eq!(account.get_balance(), 70);
    }

    #[test]
    fn test_bank_account_withdraw_insufficient() {
        let account = BankAccount::new(100);
        let result = account.withdraw(150);
        assert!(!result);
        assert_eq!(account.get_balance(), 100);
    }

    #[test]
    fn test_transfer_success() {
        let from = Arc::new(BankAccount::new(100));
        let to = Arc::new(BankAccount::new(50));

        let result = transfer(&from, &to, 30);

        assert!(result);
        assert_eq!(from.get_balance(), 70);
        assert_eq!(to.get_balance(), 80);
    }

    #[test]
    fn test_transfer_insufficient() {
        let from = Arc::new(BankAccount::new(20));
        let to = Arc::new(BankAccount::new(50));

        let result = transfer(&from, &to, 30);

        assert!(!result);
        assert_eq!(from.get_balance(), 20);
        assert_eq!(to.get_balance(), 50);
    }

    #[test]
    fn test_concurrent_deposits() {
        let account = Arc::new(BankAccount::new(0));
        let mut handles = vec![];

        for _ in 0..100 {
            let acc = Arc::clone(&account);
            handles.push(thread::spawn(move || {
                acc.deposit(1);
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(account.get_balance(), 100);
    }

    #[test]
    fn test_concurrent_transfers_no_deadlock() {
        let account1 = Arc::new(BankAccount::new(1000));
        let account2 = Arc::new(BankAccount::new(1000));
        let mut handles = vec![];

        // Multiple threads transferring in both directions
        for i in 0..20 {
            let a1 = Arc::clone(&account1);
            let a2 = Arc::clone(&account2);

            handles.push(thread::spawn(move || {
                if i % 2 == 0 {
                    transfer(&a1, &a2, 10);
                } else {
                    transfer(&a2, &a1, 10);
                }
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        // Total should remain 2000
        assert_eq!(account1.get_balance() + account2.get_balance(), 2000);
    }
}
