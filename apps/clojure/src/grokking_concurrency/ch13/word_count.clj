(ns grokking-concurrency.ch13.word-count
  (:require [clojure.string :as str]))

;; Part VIII - 分散並列処理: MapReduce WordCount

(defn map-phase
  "Map: テキストを (word, 1) ペアに変換"
  [text]
  (->> (str/split (str/lower-case text) #"\s+")
       (filter (complement str/blank?))
       (map (fn [word] [word 1]))))

(defn reduce-phase
  "Reduce: ペアを集約してワードカウント"
  [pairs]
  (reduce (fn [acc [word count]]
            (update acc word (fnil + 0) count))
          {}
          pairs))

(defn count-words-sequential
  "逐次ワードカウント"
  [texts]
  (->> texts
       (mapcat map-phase)
       reduce-phase))

(defn count-words-parallel
  "並列ワードカウント (pmap による MapReduce)"
  [texts]
  (if (empty? texts)
    {}
    (->> texts
         (pmap map-phase)
         (apply concat)
         reduce-phase)))
