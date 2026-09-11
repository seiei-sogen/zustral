---
title: 丸めをどう明示するか
labels: [wayfinder:grilling]
status: open
assignee:
blocked-by: [0006]
parent: ../map.md
---

## 問い

丸めをどういう形で言語に置くか。

- 演算を分ける（`round_half_even` と `truncate` を別の演算にする）
- 丸めモードを引数として渡す関数にする
- どのモードを標準で備えるか（half-even、half-up、truncate、ceiling、floor ほか）

init.md いわく「COBOL バグの大半はここに棲んでいる」。
暗黙に丸まる経路が1つでも残るなら、それがどこかを名指しできる状態にする。
