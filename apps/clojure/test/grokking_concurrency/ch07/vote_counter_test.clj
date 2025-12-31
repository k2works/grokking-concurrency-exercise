(ns grokking-concurrency.ch07.vote-counter-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch07.vote-counter :refer :all]))

;; Part IV - Fork/Join: VoteCounter

(deftest count-votes-sequential-test
  (testing "逐次投票カウント"
    (is (= (count-votes-sequential ["A" "B" "A" "A" "B" "C"])
           {"A" 3 "B" 2 "C" 1}))))

(deftest count-votes-parallel-test
  (testing "並列投票カウント"
    (let [votes (vec (concat (repeat 100 "A") (repeat 50 "B") (repeat 25 "C")))]
      (is (= (count-votes-parallel votes 4)
             {"A" 100 "B" 50 "C" 25})))))

(deftest merge-counts-test
  (testing "カウントのマージ"
    (is (= (merge-counts [{"A" 2 "B" 1} {"A" 1 "C" 2}])
           {"A" 3 "B" 1 "C" 2}))))

(deftest empty-votes-test
  (testing "空の投票リスト"
    (is (= (count-votes-sequential []) {}))
    (is (= (count-votes-parallel [] 4) {}))))

(deftest single-candidate-test
  (testing "単一候補"
    (is (= (count-votes-parallel ["X" "X" "X"] 2)
           {"X" 3}))))
