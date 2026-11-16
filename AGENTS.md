# AGENTS.md

コーディングエージェント向けの、このリポジトリでの作業指示。

## プロジェクト

Zustral は Austral のフォークで、業務バッチ処理と10進計算に特化したプログラミング言語である。
コンセプトは `README.md` にある。コンパイラは OCaml で書かれていて、実行ファイル名は Austral から引き継いだ `austral` のままである。

## ビルドとテスト

ツールチェーンは Nix で揃える。コマンドは `nix-shell --command '...'` の中で実行する。

- ビルド: `make`
- OCaml の単体テスト（`test/`）: `make test`
- end-to-end テスト（`test-programs/suites`）: `python test-programs/runner.py`
- コマンドラインのテスト: `python test-programs/cli-runner.py`
- 例と標準ライブラリまで含めた一通りの確認: `./run-tests.sh`

CI（`.github/workflows/build-and-test.yml`）は、これ以外にも `tools/refactoring/` のスクリプトで次を検査する。手元でも同じスクリプトを実行できる。

- 計測値が記録と一致すること（`measure.sh`）。値が変わるコミットでは `measure.sh --record` で記録し直す。
- エラーメッセージにはどれもテストがあること。
- コンパイラのどの行もテストで実行されること（`coverage.sh`）。

## 配置

- `lib/`: コンパイラ本体。組み込みモジュールは `lib/builtin/`。`lib/BuiltInModules.ml` は `concat_builtins.py` が生成するので、手で編集しない。
- `cli/`, `bin/`: コマンドラインの入口。
- `standard/`: 標準ライブラリ。
- `docs/`: 仕様（`decimal.md` など）、計画、調査。
- `.claude/skills/pstack/`, `.claude/skills/superpowers/`: コーディングエージェント用のプラグイン pstack と superpowers を取り込んだもの。上流のものをそのまま置いているので、手で編集しない。

## 設計の考え方

設計は、Scott Wlaschin の "Domain Modeling Made Functional" の発想に従う。関数型のドメイン駆動設計である。

- コード例: https://github.com/swlaschin/DomainModelingMadeFunctional

## 命名

- そっけない名前を付けない。名前だけで、何が何をするのか、どこに属するのかが分かる、説明的な名前にする。
  - 主語・述語・目的語、または動詞・目的語の形にする。
  - あるいは、大項目・中項目・小項目の形にして、何に属するかが分かるようにする。
- 名前は、コードに手を入れるたびに見直して改善する。コーディングエージェントは既存の名前に引っ張られるので、分かりにくい名前を残すと、それが新しいコードにも広がるからである。

## ディレクトリ構造

- ディレクトリは、ドメイン単位で区切る。
- 一つのディレクトリに、ファイルを無造作に入れない。まずコードの論理構造を考え、その論理構造をそのまま写した形のディレクトリ構造にする。狙いは、大規模言語モデルがコードの意味を読み取りやすくすることである。
- ディレクトリ名も、上の「命名」に従う。
- ディレクトリ構造も、コードに手を入れるたびに見直して改善する。

## 作業の進め方

- 作業は、基本的に pstack の `poteto-mode` で行う。あるいは、pstack のスキルを場面に合わせて使う。
  - pstack は `.claude/skills/pstack/` に取り込んである。セッションがこれをプラグインとして読み込んでいれば、`/pstack:poteto-mode` で呼べる。
  - 読み込んでいないセッションでは、`.claude/skills/pstack/skills/poteto-mode/SKILL.md` を読んで、それに従う。他のスキルも `.claude/skills/pstack/skills/<スキル名>/SKILL.md` にある。
  - 取り込み元は https://github.com/cursor/plugins の `harness/pstack/claude-code`（Claude Code 向けの生成版）で、版は 0.15.42、コミットは `bf0c7cd80abdbf7fe47b634978a7d1b4071f71a7` である。更新するときは、上流の同じディレクトリで丸ごと置き換え、この行の版とコミットを書き換える。
- テストについては、superpowers のテスト関連のスキルを参考にする。
  - superpowers は `.claude/skills/superpowers/` に取り込んである。セッションがこれをプラグインとして読み込んでいれば、`/superpowers:test-driven-development` のように呼べる。
  - 読み込んでいないセッションでは、`.claude/skills/superpowers/skills/<スキル名>/SKILL.md` を読んで、それに従う。テスト関連のスキルは `test-driven-development` と `verification-before-completion` である。
  - 取り込み元は https://github.com/obra/superpowers のリポジトリ全体で、版は 7.0.2、コミットは `22f0b463e7a5e579904dd6211a520321035526f7` である。更新するときは、上流のリポジトリで丸ごと置き換え、この行の版とコミットを書き換える。

## 書き方の約束

- Decimal まわりの用語は `CONTEXT.md` に従う。`_Avoid_` に挙がった言い換えは使わない。
- ドキュメント、コミットメッセージ、PR の説明は日本語で書く。
- 設計上の決定の経緯は GitHub Issues に残す（Decimal の地図は issue #9）。
