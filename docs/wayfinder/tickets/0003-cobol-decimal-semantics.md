---
title: COBOL の10進意味論のうち何を継ぐか、材料を揃える
labels: [wayfinder:research]
status: open
assignee:
blocked-by: []
parent: ../map.md
---

## 問い

COBOL が10進計算について実際に定めている規則は何か。Zustral が継ぐ価値のあるものはどれか。

掘り出す対象:

- `PIC 9(7)V99` などの記述と、それが型として何を保証しているか
- `COMPUTE` の中間結果の精度規則。処理系がどこまで規定し、どこが未定義か
- `ROUNDED` 句と、そこで選べる丸めモード（`NEAREST-EVEN`、`TRUNCATION` ほか）
- `ON SIZE ERROR` による桁あふれの扱い
- `GIVING` による結果格納先の指定と、それが暗黙に行う切り捨て

あわせて、COBOL バグが実際どこで起きているか（丸め、桁あふれ、暗黙の切り捨て）の
具体例を集める。仕様が防ぐべき対象を名指しできるようにするため。
