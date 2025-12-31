(ns grokking-concurrency.ch04.thread-basics-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch04.thread-basics :refer :all]))

;; Part II - スレッド基礎

(deftest run-in-thread-test
  (testing "スレッドでタスク実行"
    (let [result (promise)]
      (run-in-thread #(deliver result 42))
      (is (= @result 42)))))

(deftest run-multiple-threads-test
  (testing "複数スレッド実行"
    (let [results (atom [])
          threads (doall
                    (for [i (range 5)]
                      (run-in-thread #(swap! results conj i))))]
      (doseq [t threads] (.join t))
      (is (= (count @results) 5))
      (is (= (set @results) #{0 1 2 3 4})))))

(deftest future-test
  (testing "future による非同期実行"
    (let [result (run-as-future (+ 1 2))]
      (is (= @result 3)))))

(deftest multiple-futures-test
  (testing "複数の future"
    (let [futures (doall (for [i (range 5)]
                           (run-as-future (* i 2))))]
      (is (= (map deref futures) [0 2 4 6 8])))))

(deftest thread-name-test
  (testing "スレッド名の取得"
    (let [name-promise (promise)]
      (run-in-thread #(deliver name-promise (current-thread-name)))
      (is (string? @name-promise)))))
