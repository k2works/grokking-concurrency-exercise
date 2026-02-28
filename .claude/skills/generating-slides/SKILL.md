---
name: generating-slides
description: インセプションデッキの Markdown ドキュメントから PowerPoint スライド（.pptx）を生成。pptxgenjs を使用し、テンプレートのスライド構成・テーマに準拠した 12 枚構成のプレゼンテーションを出力。スライド生成やプレゼンテーション作成時に使用。
---

# スライド生成

インセプションデッキの Markdown ドキュメントから、pptxgenjs を使用して PowerPoint スライド（.pptx）を生成します。テンプレート `docs/template/インセプションデッキ.pptx` のスライド構成に準拠した 12 枚構成のプレゼンテーションを出力します。

## Instructions

### 1. 参照ドキュメント

- @docs/template/インセプションデッキ.pptx - スライド構成のリファレンス（15 枚構成、4:3、テーマカラー・フォント）

### 2. 生成スクリプト

- @.claude/scripts/generate-inception-deck.mjs - スライド生成スクリプト（**内容の編集は可能**）

### 3. 入力

- @docs/analysis/inception-deck.md - インセプションデッキ（`analyzing-inception-deck` の成果物）
- @docs/analysis/business_architecture.md - ビジネスアーキテクチャ分析書（補足情報）

### 4. 成果物

- @docs/analysis/slide/xxxxx_v0.1.0.pptx - 生成された PowerPoint スライド

### 5. 作業内容

#### 前提条件の確認

- Node.js がインストールされていること
- `pptxgenjs` パッケージがインストールされていること（未インストール時は `npm install pptxgenjs`）

#### スライド生成の実行

```bash
node .claude/scripts/generate-inception-deck.mjs
```

#### スライド構成（12 枚）

テンプレートのインセプションデッキ構成に準拠した以下の 12 枚を生成する。

| # | スライドタイトル | テンプレート対応 | データソース |
| :--- | :--- | :--- | :--- |
| 1 | タイトル | Slide 2: プロジェクト名 | プロジェクト基本情報 |
| 2 | 我われはなぜここにいるのか | Slide 3: なぜここにいるのか | なぜやるのか？ |
| 3 | エレベーターピッチ | Slide 4: エレベーターピッチ | どんなビジョンなのか？ |
| 4 | どんな価値をもたらすのか？ | Slide 5: パッケージデザイン | どんな価値をもたらすのか？ |
| 5 | やらないことリスト | Slide 6: やらないことリスト | スコープの範囲はどこか？ |
| 6 | プロジェクトコミュニティ | Slide 7: プロジェクトコミュニティ | 主なステークホルダーは？ |
| 7 | 技術的な解決策の概要 | Slide 8: 技術的な解決策 | 基本的な解決策 |
| 8 | 夜も眠れなくなるような問題 | Slide 9: 夜も眠れない問題 | 主なリスクは何か？ |
| 9 | 俺たちの "A チーム" | Slide 10: A チーム | どのくらい作業があり費用はいくらか？ |
| 10 | 期間を見極める | Slide 11: 期間を見極める | 初回リリースはいつか？ |
| 11 | トレードオフ・スライダー | Slide 12: トレードオフ | トレードオフにどう向き合うか？ |
| 12 | 初回のリリースに必要なもの | Slide 13: 初回リリース | MVP スコープ・リリース戦略 |

#### テーマ設定

テンプレートから抽出したテーマに準拠する。

- **フォント**: Yu Gothic（游ゴシック）— Windows / Mac 両対応の日本語フォント
- **テーマカラー**: ダークブルー `#333399`、ティール `#009999`、ライトティール `#BBE0E3`
- **スライドサイズ**: 4:3（10" x 7.5"）

#### 生成スクリプトの更新

inception-deck.md の内容が更新された場合、生成スクリプトの `SLIDE_DATA` オブジェクトを更新してから再実行する。

1. @docs/analysis/inception-deck.md を読み込む
2. @.claude/scripts/generate-inception-deck.mjs の `SLIDE_DATA` セクションを更新
3. `node .claude/scripts/generate-inception-deck.mjs` で再生成

#### スクリプト構成

生成スクリプトは以下の 4 セクションで構成されている。

| セクション | 内容 | 編集対象 |
| :--- | :--- | :--- |
| `SLIDE_DATA` | プロジェクト固有のデータ（テキスト・数値） | 毎回更新 |
| テーマ設定 | カラー・フォント定義 | 通常変更不要 |
| ヘルパー関数 | スライド部品の描画ロジック | 通常変更不要 |
| スライド生成 | `SLIDE_DATA` を読み取り 12 枚を生成 | 通常変更不要 |

`SLIDE_DATA` の各キーがスライドに対応している。

| キー | 対応スライド |
| :--- | :--- |
| `meta` | メタ情報（author, title, version, date, outputFileName） |
| `titleSlide` | Slide 1: タイトル |
| `whyAreWeHere` | Slide 2: 我われはなぜここにいるのか |
| `elevatorPitch` | Slide 3: エレベーターピッチ |
| `values` | Slide 4: どんな価値をもたらすのか？ |
| `scope` | Slide 5: やらないことリスト |
| `stakeholders` | Slide 6: プロジェクトコミュニティ |
| `technicalSolution` | Slide 7: 技術的な解決策の概要 |
| `risks` | Slide 8: 夜も眠れなくなるような問題 |
| `team` | Slide 9: 俺たちの "A チーム" |
| `timeline` | Slide 10: 期間を見極める |
| `tradeoffs` | Slide 11: トレードオフ・スライダー |
| `initialRelease` | Slide 12: 初回のリリースに必要なもの |

#### バージョン管理

- 出力ファイル名は `SLIDE_DATA.meta.outputFileName` で指定する（例: `PROJECT_v0.1.0.pptx`）
- `meta.title` と `meta.outputFileName` を同時に更新すること

### 6. 注意事項

- **前提条件**: @docs/analysis/inception-deck.md が作成済みであること（`analyzing-inception-deck` を先に実行）
- **制限事項**: テンプレート @docs/template/インセプションデッキ.pptx は編集しないこと。日本語フォントは Yu Gothic を使用すること（Gill Sans 等の欧文フォントは文字化けする）
- **推奨事項**: 生成後は PowerPoint で開いてレイアウトを目視確認し、必要に応じてスクリプトのレイアウトパラメータを微調整する

## Examples

### インセプションデッキのスライドを新規生成

1. `npm install pptxgenjs`（初回のみ）
2. @docs/analysis/inception-deck.md の内容を確認
3. @.claude/scripts/generate-inception-deck.mjs の `SLIDE_DATA` をプロジェクト固有の内容に書き換える
4. `node .claude/scripts/generate-inception-deck.mjs` を実行
5. @docs/analysis/slide/ に .pptx が生成されたことを確認

### インセプションデッキ更新後のスライド再生成

1. @docs/analysis/inception-deck.md の更新内容を確認
2. @.claude/scripts/generate-inception-deck.mjs の `SLIDE_DATA` を更新
3. `node .claude/scripts/generate-inception-deck.mjs` を実行

### 関連スキル

- `analyzing-inception-deck` : 入力となるインセプションデッキの作成
- `analyzing-business` : ビジネスアーキテクチャ分析（補足情報の参照元）
