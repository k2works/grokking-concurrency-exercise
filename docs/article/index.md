# Grokking Concurrency

Python、Java、C#、Scala、F#、Rust、Haskell、Clojure で学ぶ並行処理プログラミング

---

## 概要

本サイトは「Grokking Concurrency」（Kirill Bobrov 著）の学習コンパニオンとして、並行処理プログラミングの概念を複数の言語で実装しながら日本語で解説します。

---

## 言語別学習ガイド

| 言語 | 特徴 | 主要な並行処理機能 |
|------|------|-------------------|
| [Python](python/index.md) | 動的型付け、シンプル | threading, multiprocessing, asyncio |
| [Java](java/index.md) | JVM、エンタープライズ | ExecutorService, CompletableFuture |
| [C#](csharp/index.md) | .NET、モダン構文 | Task, async/await, TPL |
| [Scala](scala/index.md) | 関数型+オブジェクト指向 | Future, Akka Actor |
| [F#](fsharp/index.md) | .NET 関数型 | Async, MailboxProcessor |
| [Rust](rust/index.md) | メモリ安全、ゼロコスト | std::thread, Rayon, tokio |
| [Haskell](haskell/index.md) | 純粋関数型 | STM, async, forkIO |
| [Clojure](clojure/index.md) | Lisp 系、JVM | atom, ref/STM, core.async |

---

## 学習トピック

本シリーズでは以下のトピックを扱います：

| Part | トピック | キーワード |
|------|----------|------------|
| I | 並行処理の基礎 | 逐次処理、パフォーマンス測定 |
| II | プロセスとスレッド | Process、Thread、スレッドプール |
| III | マルチタスキング | タイムシェアリング、スケジューリング |
| IV | 並列パターン | Fork/Join、パイプライン |
| V | 同期と排他制御 | Lock、Semaphore、デッドロック |
| VI | ノンブロッキング I/O | イベントループ、Reactor |
| VII | 非同期プログラミング | asyncio、async/await |
| VIII | 分散並列処理 | MapReduce、分散処理 |

---

## 参考資料

- [Grokking Concurrency](https://www.manning.com/books/grokking-concurrency) - 原著（Manning Publications）
