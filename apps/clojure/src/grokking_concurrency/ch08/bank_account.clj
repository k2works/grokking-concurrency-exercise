(ns grokking-concurrency.ch08.bank-account)

;; Part V - 同期と排他制御: BankAccount (STM)

(defn create-account
  "銀行口座を作成"
  [id initial-balance]
  {:id id
   :balance (ref initial-balance)})

(defn get-id
  "口座IDを取得"
  [account]
  (:id account))

(defn get-balance
  "残高を取得"
  [account]
  @(:balance account))

(defn deposit!
  "入金"
  [account amount]
  (dosync
    (alter (:balance account) + amount)))

(defn withdraw!
  "出金（残高チェック付き）"
  [account amount]
  (dosync
    (if (>= @(:balance account) amount)
      (do
        (alter (:balance account) - amount)
        true)
      false)))

(defn transfer!
  "振込（STM によるデッドロックフリー）"
  [from to amount]
  (dosync
    (if (>= @(:balance from) amount)
      (do
        (alter (:balance from) - amount)
        (alter (:balance to) + amount)
        true)
      false)))
