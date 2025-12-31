package concurrency.ch08

class BankAccount(initialBalance: Int):
  private val lock = new Object
  private var balance: Int = initialBalance

  def getBalance: Int = lock.synchronized {
    balance
  }

  def deposit(amount: Int): Unit = lock.synchronized {
    balance += amount
  }

  def withdraw(amount: Int): Boolean = lock.synchronized {
    if balance >= amount then
      balance -= amount
      true
    else
      false
  }

object BankAccount:
  /** Transfer money between accounts atomically, avoiding deadlock */
  def transfer(from: BankAccount, to: BankAccount, amount: Int): Boolean =
    // Always lock in consistent order to avoid deadlock
    val (first, second) = if System.identityHashCode(from) < System.identityHashCode(to)
      then (from, to)
      else (to, from)

    first.lock.synchronized {
      second.lock.synchronized {
        if from.balance >= amount then
          from.balance -= amount
          to.balance += amount
          true
        else
          false
      }
    }
