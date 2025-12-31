package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch08.BankAccount
import java.util.concurrent.{CountDownLatch, Executors}

class BankAccountSpec extends AnyFunSuite with Matchers:

  test("BankAccount should deposit correctly"):
    val account = new BankAccount(100)
    account.deposit(50)
    account.getBalance shouldBe 150

  test("BankAccount should withdraw correctly"):
    val account = new BankAccount(100)
    val result = account.withdraw(50)
    result shouldBe true
    account.getBalance shouldBe 50

  test("BankAccount should not withdraw more than balance"):
    val account = new BankAccount(100)
    val result = account.withdraw(150)
    result shouldBe false
    account.getBalance shouldBe 100

  test("BankAccount transfer should be atomic"):
    val from = new BankAccount(100)
    val to = new BankAccount(0)

    val result = BankAccount.transfer(from, to, 50)
    result shouldBe true
    from.getBalance shouldBe 50
    to.getBalance shouldBe 50

  test("BankAccount should handle concurrent transfers safely"):
    val account1 = new BankAccount(1000)
    val account2 = new BankAccount(1000)

    val executor = Executors.newFixedThreadPool(10)
    val latch = new CountDownLatch(100)

    // 50 transfers from account1 to account2
    // 50 transfers from account2 to account1
    (1 to 50).foreach { _ =>
      executor.submit(new Runnable:
        def run(): Unit =
          BankAccount.transfer(account1, account2, 10)
          latch.countDown()
      )
      executor.submit(new Runnable:
        def run(): Unit =
          BankAccount.transfer(account2, account1, 10)
          latch.countDown()
      )
    }

    latch.await()
    executor.shutdown()

    // Total balance should be preserved
    (account1.getBalance + account2.getBalance) shouldBe 2000
