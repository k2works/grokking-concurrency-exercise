(ns grokking-concurrency.ch04.thread-basics)

;; Part II - スレッド基礎

(defn run-in-thread
  "新しいスレッドでタスクを実行"
  [task]
  (let [thread (Thread. task)]
    (.start thread)
    thread))

(defn current-thread-name
  "現在のスレッド名を取得"
  []
  (.getName (Thread/currentThread)))

(defmacro run-as-future
  "future として非同期実行"
  [& body]
  `(future ~@body))
