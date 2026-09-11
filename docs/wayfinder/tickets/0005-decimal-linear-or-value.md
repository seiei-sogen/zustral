---
title: Decimal は線形リソースか、値型か
labels: [wayfinder:grilling]
status: open
assignee:
blocked-by: [0001]
parent: ../map.md
---

## 問い

Decimal 値は Austral の線形型として扱うのか、`Float64` と同じ自由に複製できる値型にするのか。

ヒープ確保を伴うバックエンド（任意精度）を選べば、Decimal は必然的に線形リソースになる。
固定幅なら値型のままでいられる。

- 金額を線形にすると「二重計上も計上漏れも型エラー」という init.md の着眼点に近づくが、
  `a + b` のたびに消費が起きる算術は書き味として成立するのか
- 値型にした場合、簿記の線形性はどの層で与えるのか（Decimal ではなく Entry を線形にする道）
