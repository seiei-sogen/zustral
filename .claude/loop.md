今のブランチの PR を見張って、CI とレビューに対応する。
1回の周回で、次を上から順に確かめる。

GitHub は `gh api`（REST API）と `gh run` で調べる。
`gh pr view` や `gh pr checks` は GraphQL を使うので、Claude Code のクラウド環境では失敗する。

1. PR の状態を調べる。

   ```bash
   gh api "repos/{owner}/{repo}/pulls?head={owner}:{branch}&state=all" \
     --jq '.[0] | {number, state, merged_at, base: .base.ref, head_sha: .head.sha, url: .html_url}'
   ```

   `null` なら、今のブランチに PR はない。
   PR がない、または `state` が `closed`（`merged_at` があればマージ済み）なら、そのことを1行で書いて、ループを止める。

2. CI を見る。PR の先頭のコミットで動いた実行を調べる。

   ```bash
   gh run list --commit <head_sha> --json databaseId,workflowName,status,conclusion
   ```

   - 実行中（`status` が `completed` でない）なら、何もせずに手順3へ進む。
   - 落ちていたら（`conclusion` が `failure`）、`gh run view <databaseId> --log-failed` で、落ちたステップのログを読んで原因を突き止める。
     直し方とやってはいけないことは、`run-every-ci-check-locally` スキルに従う。
     いちばん小さい修正を入れ、push の前にそのスキルを実行して、最後の行が「すべて通過」になるのを確かめる。
     push したら、この周回はそこで終える。

3. まだ返信していないレビューコメントに対応する。

   ```bash
   gh api "repos/{owner}/{repo}/pulls/<PR の番号>/comments"   # コードの行へのコメント
   gh api "repos/{owner}/{repo}/pulls/<PR の番号>/reviews"    # レビューの本文
   gh api "repos/{owner}/{repo}/issues/<PR の番号>/comments"  # 会話のコメント
   ```

   - 一つずつ対応し、そのコメントに日本語で返信する。何をどう変えたか、変えなかったならその理由を書く。
     行へのコメントには `gh api -X POST "repos/{owner}/{repo}/pulls/<PR の番号>/comments/<コメントの id>/replies" -f body='…'` で返信する。
     それ以外には `gh api -X POST "repos/{owner}/{repo}/issues/<PR の番号>/comments" -f body='…'` で返信する。
   - 仕様や設計の決定を変える指摘（Decimal の意味、型の規則、言語の構文など）には、コードを変えずに返信で論点を整理し、判断を人に残す。設計の決定は GitHub Issues に残すのが、このリポジトリの決まりである。
   - スレッドを解決済みにするのは、人に任せる。

4. CI が通っていて、未対応のコメントもなければ、「PR #<番号>: CI 通過、未対応のコメントなし」と1行だけ書く。
   ほかの作業は始めない。

## 守ること

- commit のメッセージは日本語で書く。push するのは今の PR のブランチだけにする。
- force push、ベースブランチへの push、PR のマージ、PR のクローズはしない。
- 検査を弱めて CI を通さない。記録やテストの期待値の書き換え、テストの削除や skip がそれに当たる。それ以外に方法がなければ、手を止めて状況を報告する。
- 同じ失敗を2回続けて直せなかったら、それ以上は試さない。分かったことを報告して、ループを止める。
