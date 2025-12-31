(ns grokking-concurrency.ch02.password-cracker
  (:require [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]))

;; Part I - 逐次処理: PasswordCracker

(def ^:private alphabet "abcdefghijklmnopqrstuvwxyz")

(defn sha256
  "SHA256 ハッシュを計算"
  [s]
  (-> (hash/sha256 s)
      (codecs/bytes->hex)))

(defn generate-password
  "数値インデックスからパスワードを生成"
  [index length]
  (let [base (count alphabet)]
    (loop [idx index
           acc '()]
      (if (= (count acc) length)
        (apply str acc)
        (recur (quot idx base)
               (cons (nth alphabet (mod idx base)) acc))))))

(defn crack-password-range
  "範囲内でパスワードを検索"
  [target-hash start end length]
  (loop [idx start]
    (when (< idx end)
      (let [password (generate-password idx length)
            hash (sha256 password)]
        (if (= hash target-hash)
          password
          (recur (inc idx)))))))

(defn crack-password
  "パスワードをクラック（逐次処理）"
  [target-hash length]
  (let [total-combinations (long (Math/pow (count alphabet) length))]
    (crack-password-range target-hash 0 total-combinations length)))
