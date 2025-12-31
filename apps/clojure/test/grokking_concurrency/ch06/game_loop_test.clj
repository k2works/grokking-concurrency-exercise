(ns grokking-concurrency.ch06.game-loop-test
  (:require [clojure.test :refer :all]
            [grokking-concurrency.ch06.game-loop :refer :all]))

;; Part III - マルチタスキング: GameLoop

(deftest create-event-test
  (testing "イベント作成"
    (let [event (create-event)]
      (is (not (nil? event)))
      (is (false? @(:ready event))))))

(deftest signal-and-wait-test
  (testing "シグナルと待機"
    (let [event (create-event)
          result (promise)]
      (future
        (Thread/sleep 50)
        (signal-event! event))
      (wait-event! event)
      (deliver result :done)
      (is (= @result :done)))))

(deftest multiple-signals-test
  (testing "複数回のシグナル"
    (let [event (create-event)
          counter (atom 0)]
      (dotimes [_ 3]
        (signal-event! event)
        (wait-event! event)
        (swap! counter inc))
      (is (= @counter 3)))))

(deftest game-state-test
  (testing "ゲーム状態の初期化"
    (let [state (create-game-state)]
      (is (= @(:event-count state) 0))
      (is (= @(:rendered-count state) 0)))))

(deftest process-event-test
  (testing "イベント処理"
    (let [state (create-game-state)]
      (process-event! state)
      (is (= @(:event-count state) 1)))))

(deftest render-frame-test
  (testing "フレームレンダリング"
    (let [state (create-game-state)]
      (render-frame! state)
      (is (= @(:rendered-count state) 1)))))
