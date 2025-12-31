(ns grokking-concurrency.ch13.word-count-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch13.word-count :refer :all]))

;; Part VIII - 分散並列処理: MapReduce WordCount

(deftest map-phase-test
  (testing "Map フェーズ"
    (is (= (map-phase "hello world")
           [["hello" 1] ["world" 1]]))
    (is (= (map-phase "hello hello")
           [["hello" 1] ["hello" 1]]))))

(deftest reduce-phase-test
  (testing "Reduce フェーズ"
    (is (= (reduce-phase [["hello" 1] ["world" 1] ["hello" 1]])
           {"hello" 2 "world" 1}))))

(deftest count-words-sequential-test
  (testing "逐次ワードカウント"
    (is (= (count-words-sequential ["hello world" "hello clojure" "world of clojure"])
           {"hello" 2 "world" 2 "clojure" 2 "of" 1}))))

(deftest count-words-parallel-test
  (testing "並列ワードカウント"
    (is (= (count-words-parallel ["hello world" "hello clojure" "world of clojure"])
           {"hello" 2 "world" 2 "clojure" 2 "of" 1}))))

(deftest empty-input-test
  (testing "空の入力"
    (is (= (count-words-sequential []) {}))
    (is (= (count-words-parallel []) {}))))

(deftest case-insensitive-test
  (testing "大文字小文字を区別しない"
    (is (= (count-words-parallel ["Hello HELLO hello"])
           {"hello" 3}))))

(deftest large-input-test
  (testing "大量の入力"
    (let [texts (repeat 100 "word1 word2 word3")
          result (count-words-parallel texts)]
      (is (= (get result "word1") 100))
      (is (= (get result "word2") 100))
      (is (= (get result "word3") 100)))))
