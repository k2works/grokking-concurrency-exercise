namespace GrokkingConcurrency.Ch08

open System.Runtime.CompilerServices

type BankAccount = {
    Lock: obj
    mutable Balance: int
}

module BankAccount =
    /// Create a new bank account
    let createAccount (initialBalance: int) : BankAccount =
        { Lock = obj(); Balance = initialBalance }

    /// Get the current balance
    let getBalance (account: BankAccount) : int =
        lock account.Lock (fun () -> account.Balance)

    /// Deposit money into the account
    let deposit (account: BankAccount) (amount: int) : unit =
        lock account.Lock (fun () ->
            account.Balance <- account.Balance + amount
        )

    /// Withdraw money from the account
    let withdraw (account: BankAccount) (amount: int) : bool =
        lock account.Lock (fun () ->
            if account.Balance >= amount then
                account.Balance <- account.Balance - amount
                true
            else
                false
        )

    /// Transfer money between accounts atomically, avoiding deadlock
    let transfer (from: BankAccount) (toAccount: BankAccount) (amount: int) : bool =
        // Always lock in consistent order to avoid deadlock
        let fromHash = RuntimeHelpers.GetHashCode(from)
        let toHash = RuntimeHelpers.GetHashCode(toAccount)
        let (first, second) =
            if fromHash < toHash then (from, toAccount)
            else (toAccount, from)

        lock first.Lock (fun () ->
            lock second.Lock (fun () ->
                if from.Balance >= amount then
                    from.Balance <- from.Balance - amount
                    toAccount.Balance <- toAccount.Balance + amount
                    true
                else
                    false
            )
        )
