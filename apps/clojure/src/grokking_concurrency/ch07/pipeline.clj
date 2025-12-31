(ns grokking-concurrency.ch07.pipeline
  (:require [clojure.core.async :as async :refer [>!! <!! chan go >! <!]]))

;; Part IV - Pipeline

(defn create-pipeline
  "入力を2倍にするパイプラインを作成"
  []
  (let [input (chan 10)
        output (chan 10)]
    (go
      (loop []
        (when-let [v (<! input)]
          (>! output (* v 2))
          (recur))))
    {:input input
     :output output}))

(defn send-to-pipeline!
  "パイプラインにデータを送信"
  [pipeline value]
  (>!! (:input pipeline) value))

(defn receive-from-pipeline!
  "パイプラインから結果を受信"
  [pipeline]
  (<!! (:output pipeline)))

(defn run-chain-pipeline
  "チェーンパイプラインを実行"
  [initial-value functions]
  (if (empty? functions)
    initial-value
    (let [channels (repeatedly (inc (count functions)) #(chan 1))]
      ;; 各ステージを設定
      (doseq [[in-ch out-ch f] (map vector channels (rest channels) functions)]
        (go
          (when-let [v (<! in-ch)]
            (>! out-ch (f v)))))
      ;; 初期値を送信
      (>!! (first channels) initial-value)
      ;; 結果を受信
      (<!! (last channels)))))
