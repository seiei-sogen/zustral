# AGENTS.md

コーディングエージェント向けの、このリポジトリでの作業指示。

## プロジェクト

Zustral は Austral のフォークで、業務バッチ処理と10進計算に特化したプログラミング言語である。
コンセプトは `README.md` にある。コンパイラは OCaml で書かれていて、実行ファイル名は `zustral` である。

## ビルドとテスト

ツールチェーンは Nix で揃える。コマンドは `nix-shell --command '...'` の中で実行する。
Claude Code のクラウド環境では、先に `tools/coding-agent-setup/fetch-pinned-nixpkgs-with-git.sh` を1回実行する。nixpkgs の tarball を取得できないので、同じタグを git で取得する（issue #42）。

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
- `tools/coding-agent-setup/`: Claude Code のプラグインとスキルを入れるスクリプト。クラウド環境のセットアップスクリプトから呼ぶ。
- `.claude/skills/cc-init-plugin-install/`: 上のスクリプトを実行するスキル。`/cc-init-plugin-install` で呼ぶ。
- `.claude/skills/run-every-ci-check-locally/`: CI と同じ検査を手元ですべて実行し、落ちた検査の直し方を示すスキル。`/goal` の完了条件にも使う。
- `.claude/loop.md`: 引数なしの `/loop` が周回ごとに実行する指示。今のブランチの PR の CI とレビューに対応する。

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

## ファイルの分け方

- 一つのファイルに、関数をいくつも詰め込まない。これは AI がやりがちなことである。原則は、1ファイルに1関数である。
- 1ファイルは、原則として、コメントなどを除いて200行以内に収める。

## 書き方の約束

- Decimal まわりの用語は `CONTEXT.md` に従う。`_Avoid_` に挙がった言い換えは使わない。
- ドキュメント、コミットメッセージ、PR の説明は日本語で書く。
- 設計上の決定の経緯は GitHub Issues に残す（Decimal の地図は issue #9）。

## 作業の進め方

- 作業は、`/pstack:poteto-mode` で進める。
- テストは、`/superpowers:test-driven-development` に従って書く。
