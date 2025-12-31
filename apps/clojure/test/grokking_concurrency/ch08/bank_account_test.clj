(ns grokking-concurrency.ch08.bank-account-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch08.bank-account :refer :all]))

;; Part V - 同期と排他制御: BankAccount

(deftest create-account-test
  (testing "アカウント作成"
    (let [account (create-account 1 1000)]
      (is (= (get-id account) 1))
      (is (= (get-balance account) 1000)))))

(deftest deposit-test
  (testing "入金"
    (let [account (create-account 1 1000)]
      (deposit! account 500)
      (is (= (get-balance account) 1500)))))

(deftest withdraw-test
  (testing "出金"
    (let [account (create-account 1 1000)]
      (is (true? (withdraw! account 300)))
      (is (= (get-balance account) 700)))))

(deftest withdraw-insufficient-test
  (testing "残高不足"
    (let [account (create-account 1 1000)]
      (is (false? (withdraw! account 1500)))
      (is (= (get-balance account) 1000)))))

(deftest transfer-test
  (testing "振込"
    (let [from (create-account 1 1000)
          to (create-account 2 500)]
      (is (true? (transfer! from to 300)))
      (is (= (get-balance from) 700))
      (is (= (get-balance to) 800)))))

(deftest transfer-insufficient-test
  (testing "振込 - 残高不足"
    (let [from (create-account 1 100)
          to (create-account 2 500)]
      (is (false? (transfer! from to 200)))
      (is (= (get-balance from) 100))
      (is (= (get-balance to) 500)))))

(deftest concurrent-transfers-test
  (testing "並行振込 - 合計金額保持"
    (let [a1 (create-account 1 1000)
          a2 (create-account 2 1000)
          initial-total 2000
          iterations 100
          futures (doall
                    (for [_ (range iterations)]
                      (future
                        (transfer! a1 a2 10)
                        (transfer! a2 a1 10))))]
      (doseq [f futures] @f)
      (is (= (+ (get-balance a1) (get-balance a2)) initial-total)))))

(deftest no-deadlock-test
  (testing "デッドロックなし"
    (let [a1 (create-account 1 1000)
          a2 (create-account 2 1000)
          f1 (future (dotimes [_ 50] (transfer! a1 a2 1)))
          f2 (future (dotimes [_ 50] (transfer! a2 a1 1)))]
      @f1 @f2
      (is (= (+ (get-balance a1) (get-balance a2)) 2000)))))
