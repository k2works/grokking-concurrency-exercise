# Grokking Concurrency Exercise

Python、Java、C#、Scala、F#、Rust、Haskell、Clojure で学ぶ並行処理プログラミング

## 概要

本プロジェクトは「Grokking Concurrency」（Kirill Bobrov 著）の学習コンパニオンとして、並行処理プログラミングの概念を複数の言語で実装しています。

### 目的

- 並行処理の基本概念を理解する
- 各言語の並行処理機能を比較学習する
- 実際のコードを通じて並行処理パターンを習得する

### 対応言語

| 言語 | 特徴 | テスト数 | 主要な並行処理機能 |
|------|------|---------|-------------------|
| Python | 動的型付け、シンプル | - | threading, multiprocessing, asyncio |
| Java | JVM、エンタープライズ | 36 | ExecutorService, CompletableFuture |
| C# | .NET、モダン構文 | 36 | Task, async/await, TPL |
| Scala | 関数型+オブジェクト指向 | 35 | Future, Akka Actor |
| F# | .NET 関数型 | 35 | Async, MailboxProcessor |
| Rust | メモリ安全、ゼロコスト | 36 | std::thread, Rayon, tokio |
| Haskell | 純粋関数型 | 36 | STM, async, forkIO |
| Clojure | Lisp 系、JVM | 45 | atom, ref/STM, core.async |

## 構成

```
grokking-concurrency-exercise/
├── apps/
│   ├── python/     # Python 実装
│   ├── java/       # Java 実装
│   ├── csharp/     # C# 実装
│   ├── scala/      # Scala 実装
│   ├── fsharp/     # F# 実装
│   ├── rust/       # Rust 実装
│   ├── haskell/    # Haskell 実装
│   └── clojure/    # Clojure 実装
└── docs/
    └── article/    # MkDocs ドキュメント
```

## Quick Start

### ドキュメントの閲覧

```bash
# MkDocs サーバーを起動
npm run docs:serve
# または
docker-compose up mkdocs
```

ブラウザで http://localhost:8000 にアクセス

### 各言語のテスト実行

```bash
# Java
cd apps/java && ./gradlew test

# C#
cd apps/csharp && dotnet test

# Scala
cd apps/scala && sbt test

# F#
cd apps/fsharp && dotnet test

# Rust
cd apps/rust && cargo test

# Haskell
cd apps/haskell && stack test

# Clojure
cd apps/clojure && lein test
```

## 学習トピック

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

## 参考資料

- [Grokking Concurrency](https://www.manning.com/books/grokking-concurrency) - 原著（Manning Publications）
- [GitHub Pages ドキュメント](https://k2works.github.io/grokking-concurrency-exercise/)

## ライセンス

MIT License
