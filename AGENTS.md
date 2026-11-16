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

## 書き方の約束

- Decimal まわりの用語は `CONTEXT.md` に従う。`_Avoid_` に挙がった言い換えは使わない。
- ドキュメント、コミットメッセージ、PR の説明は日本語で書く。
- 設計上の決定の経緯は GitHub Issues に残す（Decimal の地図は issue #9）。
