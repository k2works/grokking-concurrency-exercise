package concurrency.ch08;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Timeout;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.assertj.core.api.Assertions.*;

/**
 * 銀行口座のテスト（同期と排他制御）
 */
class BankAccountTest {

    @Test
    @DisplayName("口座に入金できる")
    void shouldDeposit() {
        BankAccount account = new BankAccount(100);

        account.deposit(50);

        assertThat(account.getBalance()).isEqualTo(150);
    }

    @Test
    @DisplayName("口座から出金できる")
    void shouldWithdraw() {
        BankAccount account = new BankAccount(100);

        boolean result = account.withdraw(50);

        assertThat(result).isTrue();
        assertThat(account.getBalance()).isEqualTo(50);
    }

    @Test
    @DisplayName("残高不足では出金できない")
    void shouldNotWithdrawWithInsufficientFunds() {
        BankAccount account = new BankAccount(100);

        boolean result = account.withdraw(150);

        assertThat(result).isFalse();
        assertThat(account.getBalance()).isEqualTo(100);
    }

    @Test
    @DisplayName("並行アクセスでも残高が正確")
    @Timeout(10)
    void shouldMaintainBalanceUnderConcurrentAccess() throws InterruptedException {
        BankAccount account = new BankAccount(1000);
        int numThreads = 100;
        int depositsPerThread = 10;
        CountDownLatch latch = new CountDownLatch(numThreads);

        try (ExecutorService executor = Executors.newFixedThreadPool(10)) {
            for (int i = 0; i < numThreads; i++) {
                executor.submit(() -> {
                    for (int j = 0; j < depositsPerThread; j++) {
                        account.deposit(1);
                    }
                    latch.countDown();
                });
            }
            latch.await();
        }

        // 1000 + (100 * 10) = 2000
        assertThat(account.getBalance()).isEqualTo(2000);
    }

    @Test
    @DisplayName("口座間で送金できる")
    void shouldTransferBetweenAccounts() {
        BankAccount from = new BankAccount(100);
        BankAccount to = new BankAccount(50);

        boolean result = BankAccount.transfer(from, to, 30);

        assertThat(result).isTrue();
        assertThat(from.getBalance()).isEqualTo(70);
        assertThat(to.getBalance()).isEqualTo(80);
    }
}
