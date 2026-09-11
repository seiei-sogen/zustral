---
title: Austral の型パラメータ機構に値パラメータを足す道筋を掴む
labels: [wayfinder:research]
status: open
assignee:
blocked-by: []
parent: ../map.md
---

## 問い

`Decimal[7, 2]` のように型が値（精度とスケール）を取れるようにするには、
このコンパイラのどこに何が要るのか。

現状 Austral のジェネリクスは型パラメータのみを取る。調べる先:

- 型の内部表現（`lib/Type.ml`）と型パラメータの扱い
- 構文解析（`lib/CstParser`、型構文まわり）
- 型検査・単一化・置換（`lib/TypeSystem.ml`、`lib/TypeParser.ml` 相当）
- 環境と宣言の登録（`lib/Env.ml`）、モジュールインターフェースでの露出
- コード生成（`lib/CodeGen.ml`、`lib/CRenderer.ml`）で単相化がどう行われているか

知りたいのは「作業量の見積もり」ではなく「変更が波及する面の形」。
値パラメータを避けて設計する道（スケールを値に持たせる）と比べるための材料を揃える。
