(ns grokking-concurrency.ch06.game-loop)

;; Part III - マルチタスキング: GameLoop

(defn create-event
  "イベントオブジェクトを作成"
  []
  {:ready (atom false)
   :lock (Object.)})

(defn signal-event!
  "イベントをシグナル"
  [event]
  (locking (:lock event)
    (reset! (:ready event) true)
    (.notifyAll (:lock event))))

(defn wait-event!
  "イベントを待機"
  [event]
  (locking (:lock event)
    (while (not @(:ready event))
      (.wait (:lock event)))
    (reset! (:ready event) false)))

(defn create-game-state
  "ゲーム状態を作成"
  []
  {:event-count (atom 0)
   :rendered-count (atom 0)
   :running (atom true)})

(defn process-event!
  "イベントを処理"
  [state]
  (swap! (:event-count state) inc))

(defn render-frame!
  "フレームをレンダリング"
  [state]
  (swap! (:rendered-count state) inc))

(defn run-game-loop!
  "ゲームループを実行"
  [state iterations]
  (let [processor-event (create-event)
        renderer-event (create-event)]
    ;; イベント処理スレッド
    (future
      (dotimes [_ iterations]
        (wait-event! processor-event)
        (process-event! state)
        (signal-event! renderer-event)))
    ;; レンダリングスレッド
    (future
      (dotimes [_ iterations]
        (signal-event! processor-event)
        (wait-event! renderer-event)
        (render-frame! state)))
    ;; 完了待機
    (Thread/sleep (* iterations 10))
    state))
