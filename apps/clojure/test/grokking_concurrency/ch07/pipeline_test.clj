(ns grokking-concurrency.ch07.pipeline-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch07.pipeline :refer :all]
            [clojure.core.async :as async]))

;; Part IV - Pipeline

(deftest create-pipeline-test
  (testing "パイプライン作成"
    (let [pipeline (create-pipeline)]
      (is (not (nil? (:input pipeline))))
      (is (not (nil? (:output pipeline)))))))

(deftest simple-pipeline-test
  (testing "シンプルなパイプライン"
    (let [pipeline (create-pipeline)]
      (send-to-pipeline! pipeline 5)
      (is (= (receive-from-pipeline! pipeline) 10)))))

(deftest multiple-items-test
  (testing "複数アイテムの処理"
    (let [pipeline (create-pipeline)]
      (doseq [i [1 2 3]]
        (send-to-pipeline! pipeline i))
      (is (= (receive-from-pipeline! pipeline) 2))
      (is (= (receive-from-pipeline! pipeline) 4))
      (is (= (receive-from-pipeline! pipeline) 6)))))

(deftest chain-pipeline-test
  (testing "チェーンパイプライン"
    (let [result (run-chain-pipeline 5 [#(* % 2) #(+ % 3) #(* % 10)])]
      (is (= result 130)))))

(deftest empty-chain-test
  (testing "空のチェーン"
    (is (= (run-chain-pipeline 5 []) 5))))
