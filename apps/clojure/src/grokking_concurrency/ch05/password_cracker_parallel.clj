(ns grokking-concurrency.ch05.password-cracker-parallel
  (:require [grokking-concurrency.ch02.password-cracker :as seq-cracker]))

;; Part II - 並列パスワードクラック

(defn chunk-range
  "範囲をチャンクに分割"
  [start end num-chunks]
  (let [size (- end start)
        chunk-size (long (Math/ceil (/ size num-chunks)))]
    (loop [current start
           chunks []]
      (if (>= current end)
        chunks
        (let [next-end (min (+ current chunk-size) end)]
          (recur next-end (conj chunks [current next-end])))))))

(defn crack-password-parallel
  "並列でパスワードをクラック"
  [target-hash length num-threads]
  (let [total-combinations (long (Math/pow (count "abcdefghijklmnopqrstuvwxyz") length))
        chunks (chunk-range 0 total-combinations num-threads)
        futures (doall
                  (for [[start end] chunks]
                    (future
                      (seq-cracker/crack-password-range target-hash start end length))))]
    (some identity (map deref futures))))
