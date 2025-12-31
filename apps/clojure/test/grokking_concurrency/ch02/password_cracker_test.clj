(ns grokking-concurrency.ch02.password-cracker-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch02.password-cracker :refer :all]))

;; Part I - 逐次処理: PasswordCracker

(deftest sha256-hash-test
  (testing "SHA256 ハッシュ計算"
    (is (= (sha256 "abc")
           "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (is (= (sha256 "test")
           "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"))))

(deftest generate-password-test
  (testing "パスワード生成"
    (is (= (generate-password 0 4) "aaaa"))
    (is (= (generate-password 1 4) "aaab"))
    (is (= (generate-password 25 4) "aaaz"))
    (is (= (generate-password 26 4) "aaba"))))

(deftest crack-password-range-test
  (testing "パスワード範囲検索"
    (let [target-hash (sha256 "aaab")]
      (is (= (crack-password-range target-hash 0 100 4) "aaab")))
    (let [target-hash (sha256 "aaba")]
      (is (= (crack-password-range target-hash 0 100 4) "aaba")))))

(deftest crack-password-not-found-test
  (testing "パスワードが見つからない場合"
    (is (nil? (crack-password-range "invalidhash" 0 10 4)))))

(deftest crack-password-test
  (testing "パスワードクラック - 逐次処理"
    (let [target-hash (sha256 "aabb")]
      (is (= (crack-password target-hash 4) "aabb")))))
