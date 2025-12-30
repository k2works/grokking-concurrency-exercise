package concurrency.ch08;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * 銀行口座 - 同期と排他制御の例
 * Lock を使ったスレッドセーフな実装
 */
public class BankAccount {
    private final Lock lock = new ReentrantLock();
    private int balance;

    public BankAccount(int initialBalance) {
        this.balance = initialBalance;
    }

    /**
     * 入金
     */
    public void deposit(int amount) {
        lock.lock();
        try {
            balance += amount;
        } finally {
            lock.unlock();
        }
    }

    /**
     * 出金
     */
    public boolean withdraw(int amount) {
        lock.lock();
        try {
            if (balance >= amount) {
                balance -= amount;
                return true;
            }
            return false;
        } finally {
            lock.unlock();
        }
    }

    /**
     * 残高照会
     */
    public int getBalance() {
        lock.lock();
        try {
            return balance;
        } finally {
            lock.unlock();
        }
    }

    /**
     * 口座間送金（デッドロック回避付き）
     */
    public static boolean transfer(BankAccount from, BankAccount to, int amount) {
        // デッドロック回避のため、常に同じ順序でロックを取得
        BankAccount first = System.identityHashCode(from) < System.identityHashCode(to) ? from : to;
        BankAccount second = first == from ? to : from;

        first.lock.lock();
        try {
            second.lock.lock();
            try {
                if (from.balance >= amount) {
                    from.balance -= amount;
                    to.balance += amount;
                    return true;
                }
                return false;
            } finally {
                second.lock.unlock();
            }
        } finally {
            first.lock.unlock();
        }
    }

    public static void main(String[] args) throws InterruptedException {
        BankAccount account = new BankAccount(1000);

        Thread t1 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                account.deposit(1);
            }
        });

        Thread t2 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                account.withdraw(1);
            }
        });

        t1.start();
        t2.start();
        t1.join();
        t2.join();

        System.out.println("Final balance: " + account.getBalance());
    }
}
