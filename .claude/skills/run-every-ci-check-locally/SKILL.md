---
name: run-every-ci-check-locally
description: Zustral のリポジトリで、CI（.github/workflows/build-and-test.yml）と同じ検査を手元ですべて実行し、検査ごとの合否と、最後の1行に全体の判定を報告する。変更が終わったと言う前、commit や push の前、/goal の完了条件や /loop の周回で、変更が CI を通るかを確かめるときに使う。落ちた検査の直し方と、検査を弱めて通してはいけない理由もここにある。
---

# CI と同じ検査を手元ですべて実行する

`tools/refactoring/run-every-ci-check-locally.sh` を nix-shell の中で実行し、結果を報告する。
検査の一覧は、このスクリプトにある。CI に検査を足したら、スクリプトにも足す。

このスキルは、アプリを利用者のように動かして実証する pstack の `verify-*` スキルとは別物である。
こちらは、CI の門を手元で先に通すためのものである。

## 手順

1. リポジトリのルートに移る。

   ```bash
   cd "$(git rev-parse --show-toplevel)"
   ```

2. nix-shell が使えるかを確かめる。

   ```bash
   nix-shell --command true
   ```

   Claude Code のクラウド環境で nixpkgs を取得できずに失敗したら、`tools/coding-agent-setup/fetch-pinned-nixpkgs-with-git.sh` を1回実行してから、やり直す（issue #42）。
   それでも失敗したら、エラーの最後の数行を報告して終える。
   検査を nix-shell の外で代わりに実行しない。OCaml や Python の版が CI と違うと、結果が当てにならないからである。

3. 検査をすべて実行する。ログの置き場を先に決めて渡す。

   ```bash
   log_dir="$(mktemp -d)"
   ZUSTRAL_CI_CHECK_LOG_DIR="$log_dir" nix-shell --command 'tools/refactoring/run-every-ci-check-locally.sh'
   ```

   検査は14個ある。カバレッジの検査は、計測用にコンパイラをもう一度ビルドするので時間がかかる。
   Bash ツールのタイムアウトを最大にするか、バックグラウンドで実行して終わるのを待つ。
   スクリプトは検査ごとに `ok` か `FAIL` で始まる1行を出し、1つでも落ちると終了コード 1 で終わる。

4. 今のブランチに PR があれば、ベースブランチとの等価性の検査も実行する。
   CI では PR のときだけ動く検査で、上のスクリプトには入っていない。
   PR がなければ、この手順を飛ばす。

   ```bash
   base_branch="$(gh pr view --json baseRefName --jq .baseRefName)"
   git fetch --depth=1 origin "$base_branch"
   nix-shell --command 'tools/refactoring/check.sh FETCH_HEAD --quick'
   ```

   リポジトリにあるすべてのプログラムを、ベースのコンパイラと今のコンパイラでコンパイルし、出力を比べる。
   `equivalence with FETCH_HEAD` の行の読み方は次のとおりである。

   - `ok`: 出力は変わっていない。
   - `FAIL`: どれかのプログラムで出力が変わった。リファクタリングの PR なら、差は 0 でなければならないので直す。バグ修正の PR なら、変わるのが正しい。変わったプログラムを報告に挙げ、失敗とは数えない。CI もこの差を警告として扱い、ジョブを落とさない。

   終了コード 2 は、コンパイラをビルドまたは実行できず、比較できなかったことを表す。これは失敗と数える。
   ほかの行（metrics など）は手順3と同じ検査なので、そちらの結果を使う。

5. `FAIL` の検査があれば、その行に出たログを読み、原因を1〜2行にまとめる。
   直し方は、次の節に従う。

6. 次を報告する。

   - スクリプトが出した `ok` と `FAIL` の行を、そのまま全部。
   - `FAIL` の検査ごとに、原因のまとめ。
   - 等価性の検査を実行したなら、その結果と、出力が変わったプログラム。
   - 最後の1行に、次のどちらかを書く。/goal の完了判定は、この行を見る。
     - `CI と同じ検査: すべて通過`
     - `CI と同じ検査: 失敗あり（<落ちた検査の名前を、読点で区切って並べる>）`

途中の確認では、スクリプトの中のコマンドを1つだけ nix-shell で実行してよい。
ただし「すべて通過」と書いてよいのは、手順3のスクリプトを最後まで実行して、全部が `ok` だったときだけである。

## 落ちた検査の直し方

| 検査 | 見ていること | 直す向き |
| --- | --- | --- |
| `make`, `make-test`, `end-to-end`, `command-line`, `examples`, `stdlib-build`, `stdlib-test` | ビルドとテストが通ること | コードを直す。テストの期待値を変えてよいのは、変更の目的が出力を変えることだと説明できるときだけ。 |
| `metrics-equal-the-record` | リファクタリング計画が減らしたい数が、`tools/refactoring/expected/` の記録と一致すること | 数が減ったなら `measure.sh --record` で記録し直す。増えたなら、その増加を変更の目的から説明できるときだけ記録し直し、理由をコミットメッセージに書く。説明できなければ、コードの方を直す。 |
| `no-unused-exports` | インターフェースが、どこからも使われない値を export していないこと | 使っていない export を消すか、死んだコードを消す。記録は空のまま保つ。 |
| `every-message-has-a-test` | エラーメッセージの文言が、どれかのテストの期待出力に現れること | 新しいメッセージを出すテストを書く。記録（`expected/unpinned-messages.txt`）に足してよいのは、どのプログラムからも届かない文言だけで、その理由を issue #124 に書く。 |
| `no-error-escapes-a-stage` | ステージのエラーが例外のまま外に出ず、`lib/Outcome.ml` の値として返ること | 送出する関数を `Outcome.catch (fun ...)` の中で呼ぶ。詳しい規則は `tools/refactoring/escape-check.py` の冒頭にある。 |
| `only-environment-makes-ids` | ID を作るのが `lib/Env.ml` だけであること | `Supply.initial` や `fresh_<kind>_id` を直接呼ばず、`Env` を通す。 |
| `every-wording-is-shown` | `lib/*Errors.ml` の文言の行を、どれかのプログラムが実行すること | その文言を出すプログラムをテストに足す。 |
| `every-line-is-executed` | コンパイラのどの行も、どれかのテストが実行すること | テストを足すか、届かないコードを消す。記録（`expected/unexecuted-lines.tsv`）は空で、空のまま保つ。行を足すのではなく、テストを足す。 |

### やってはいけないこと

- 記録やテストの期待値を書き換えて、検査を黙らせること。上の表で許した場合だけ書き換える。
- テストを消すこと、飛ばすこと、検査をスクリプトから外すこと。
- `lib/BuiltInModules.ml` を手で直すこと。`concat_builtins.py` が生成する。

検査を弱める以外に通す方法がないなら、手を止めて、その状況を報告する。
