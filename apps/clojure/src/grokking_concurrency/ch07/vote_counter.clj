(ns grokking-concurrency.ch07.vote-counter)

;; Part IV - Fork/Join: VoteCounter

(defn count-votes-sequential
  "逐次で投票をカウント"
  [votes]
  (reduce (fn [acc vote]
            (update acc vote (fnil inc 0)))
          {}
          votes))

(defn merge-counts
  "複数のカウント結果をマージ"
  [counts]
  (reduce (fn [acc m]
            (merge-with + acc m))
          {}
          counts))

(defn count-votes-parallel
  "並列で投票をカウント（Fork/Join パターン）"
  [votes num-workers]
  (if (empty? votes)
    {}
    (let [chunks (partition-all (max 1 (quot (count votes) num-workers)) votes)
          futures (doall (map #(future (count-votes-sequential %)) chunks))
          results (map deref futures)]
      (merge-counts results))))
