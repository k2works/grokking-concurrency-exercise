(ns grokking-concurrency.ch05.password-cracker-parallel-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch05.password-cracker-parallel :refer :all]
            [grokking-concurrency.ch02.password-cracker :as seq-cracker]))

;; Part II - 並列パスワードクラック

(deftest crack-password-parallel-test
  (testing "並列パスワードクラック"
    (let [target-hash (seq-cracker/sha256 "aabb")]
      (is (= (crack-password-parallel target-hash 4 4) "aabb")))))

(deftest crack-password-parallel-not-found-test
  (testing "パスワードが見つからない場合"
    (is (nil? (crack-password-parallel "invalidhash" 4 4)))))

(deftest chunk-range-test
  (testing "範囲のチャンク分割"
    (is (= (chunk-range 0 100 4)
           [[0 25] [25 50] [50 75] [75 100]]))
    (is (= (chunk-range 0 10 3)
           [[0 4] [4 8] [8 10]]))))

(deftest parallel-faster-test
  (testing "並列処理が機能する"
    (let [target-hash (seq-cracker/sha256 "aacz")]
      (is (= (crack-password-parallel target-hash 4 4) "aacz")))))
