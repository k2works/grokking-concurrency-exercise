module Ch08Spec (spec) where

import Test.Hspec
import Control.Concurrent.Async (mapConcurrently)
import Ch08.BankAccount

spec :: Spec
spec = describe "Ch08.BankAccount" $ do
    describe "newBankAccount" $ do
        it "creates account with initial balance" $ do
            account <- newBankAccount 1 100
            balance <- getBalance account
            balance `shouldBe` 100

    describe "deposit" $ do
        it "increases balance" $ do
            account <- newBankAccount 2 100
            deposit account 50
            balance <- getBalance account
            balance `shouldBe` 150

    describe "withdraw" $ do
        it "decreases balance on success" $ do
            account <- newBankAccount 3 100
            result <- withdraw account 30
            result `shouldBe` True
            balance <- getBalance account
            balance `shouldBe` 70

        it "fails when insufficient funds" $ do
            account <- newBankAccount 4 100
            result <- withdraw account 150
            result `shouldBe` False
            balance <- getBalance account
            balance `shouldBe` 100

    describe "transfer" $ do
        it "transfers money between accounts" $ do
            from <- newBankAccount 5 100
            to <- newBankAccount 6 50
            result <- transfer from to 30
            result `shouldBe` True
            fromBalance <- getBalance from
            toBalance <- getBalance to
            fromBalance `shouldBe` 70
            toBalance `shouldBe` 80

        it "fails when insufficient funds" $ do
            from <- newBankAccount 7 20
            to <- newBankAccount 8 50
            result <- transfer from to 30
            result `shouldBe` False
            fromBalance <- getBalance from
            toBalance <- getBalance to
            fromBalance `shouldBe` 20
            toBalance `shouldBe` 50

    describe "concurrent operations" $ do
        it "handles concurrent deposits correctly" $ do
            account <- newBankAccount 9 0
            _ <- mapConcurrently (\_ -> deposit account 1) [1..100]
            balance <- getBalance account
            balance `shouldBe` 100

        it "handles concurrent transfers without deadlock" $ do
            account1 <- newBankAccount 10 1000
            account2 <- newBankAccount 11 1000
            -- Transfer in both directions
            _ <- mapConcurrently (\i ->
                if even i
                    then transfer account1 account2 10
                    else transfer account2 account1 10
                ) [1..20]
            balance1 <- getBalance account1
            balance2 <- getBalance account2
            -- Total should remain 2000
            (balance1 + balance2) `shouldBe` 2000
