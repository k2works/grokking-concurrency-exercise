module Ch08.BankAccount
    ( BankAccount
    , newBankAccount
    , getBalance
    , deposit
    , withdraw
    , transfer
    ) where

import Control.Concurrent.STM

-- | A thread-safe bank account using STM
data BankAccount = BankAccount
    { accountId      :: Int
    , accountBalance :: TVar Int
    }

-- | Create a new bank account with initial balance
newBankAccount :: Int -> Int -> IO BankAccount
newBankAccount accId initialBalance = do
    balance <- newTVarIO initialBalance
    return $ BankAccount accId balance

-- | Get the current balance
getBalance :: BankAccount -> IO Int
getBalance account = readTVarIO (accountBalance account)

-- | Deposit money into the account
deposit :: BankAccount -> Int -> IO ()
deposit account amount = atomically $
    modifyTVar' (accountBalance account) (+ amount)

-- | Withdraw money from the account
withdraw :: BankAccount -> Int -> IO Bool
withdraw account amount = atomically $ do
    balance <- readTVar (accountBalance account)
    if balance >= amount
        then do
            writeTVar (accountBalance account) (balance - amount)
            return True
        else return False

-- | Transfer money between accounts atomically (deadlock-free with STM)
transfer :: BankAccount -> BankAccount -> Int -> IO Bool
transfer from to amount = atomically $ do
    fromBalance <- readTVar (accountBalance from)
    if fromBalance >= amount
        then do
            modifyTVar' (accountBalance from) (subtract amount)
            modifyTVar' (accountBalance to) (+ amount)
            return True
        else return False
