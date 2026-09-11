---
title: Zustral の Decimal 言語機能の設計仕様
labels: [wayfinder:map]
---

## 目的地

Zustral（Austral フォーク）に10進数を持ち込む言語仕様を、実装に着手できる粒度まで固める。
型と算術の意味論に加え、実行時表現とバックエンドの選択まで含む。実装そのものは含まない。

## メモ

- ドメイン: プログラミング言語設計（型システム、算術意味論）、10進浮動小数点、COBOL の金融計算
- 毎セッションで参照するスキル: `grilling-jp`、`domain-modeling`
- 決まりごと: Austral の哲学（暗黙変換なし・例外なし・GC なし・線形型・トラップ演算）を破る提案は、
  破る理由を明示して初めて検討する
- upstream Austral への還元は考えない。Zustral として独自に進化させる
- 現状の事実: コンパイラは OCaml（`lib/`）、stdlib は `standard/src/` の9モジュールのみ、
  分割コンパイル未対応（README:207）、ジェネリクスは型パラメータのみで値パラメータなし

### トラッカーの約束（local-markdown）

GitHub Issues がこのフォークでは無効なため、地図とチケットはこのディレクトリに置く。

- 地図: `docs/wayfinder/map.md` / チケット: `docs/wayfinder/tickets/NNNN-*.md`
- **確保**: front matter の `assignee` を埋める（作業より先に）
- **解消**: `status: closed` にし、本文末尾に `## 解消` 節として答えを書く
- **ブロック関係**: `blocked-by: [0001, 0003]`（チケット番号）
- **フロンティア**: `status: open` かつ `assignee` が空かつ `blocked-by` が全部 closed
  - `grep -H 'status:\|assignee:\|blocked-by:' docs/wayfinder/tickets/*.md`

## ここまでの決定

<!-- クローズしたチケット1件につき1行 -->

（まだない）

## 未特定

- **リテラルと型の境界**: `1.23` をどう decimal として型付けするか。整数リテラル・`Float64` との変換を
  どこまで明示に要求するか。スケールの置き場所が決まると輪郭が出る
- **比較と typeclass**: `Equality` / `Order` のインスタンスをどう与えるか。スケール違いの値は比較できるのか
- **文字列化とパース**: COBOL の編集項目（`PIC ZZZ,ZZ9.99`）相当をどこに置くか。言語側か stdlib 側か
- **設計を裏取りするプロトタイプ**: OCaml コンパイラのどこまで触れば設計の妥当性が確かめられるか
- **検証の置き場**: 既存の `test-programs/` の枠組みで10進の意味論をどう golden test にするか

## スコープ外

- **セルフホスティング**: 目的地の遥か先。stdlib（マップ・ファイルIO）と分割コンパイルが前提になる
- **複式簿記の線形リソース / Money 型**: decimal の土台が固まってから別の地図として引き直す
- **分割コンパイル、パッケージマネージャ、ビルドシステム**: upstream の ROADMAP 課題であって decimal 仕様の一部ではない
- **Zenn 連載などのコンテンツ化**: 仕様が固まった後の別作業
