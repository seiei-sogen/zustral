---
name: cc-init-plugin-install
description: Claude Code のプラグイン（pstack、superpowers、mattpocock-skills、ponytail）と seiei-sogen/agent-skills のスキルを、リポジトリのスクリプトでまとめて入れる。セッションを作った直後の初期設定や、プラグイン・スキルが入っていない、入れ直したい、更新したいと言われたときに使う。
---

# Claude Code のプラグインとスキルを入れる

`tools/coding-agent-setup/install-claude-code-plugins-and-skills.sh` を実行し、結果を報告する。
入れるものの一覧は、このスクリプトの先頭にある。一覧を変える依頼なら、スクリプトを編集する。

## 手順

1. リポジトリのルートで、スクリプトを1回実行する。

   ```bash
   bash "$(git rev-parse --show-toplevel)/tools/coding-agent-setup/install-claude-code-plugins-and-skills.sh"
   ```

   何度実行してもよい。入っているプラグインは入れ直さず、スキルは最新の内容で置き換える。

2. 終了コードが 0 でなければ、失敗したコマンドとその出力を報告して終える。
   別の方法でのインストールは、ユーザーに頼まれたときだけ行う。

3. 成功したら、`claude plugin list` で入ったプラグインを確かめる。

4. 次を報告する。

   - 入ったプラグインの名前と版。
   - スキルの置き場に入った個数（スクリプトの最後の行に出る）。
   - スクリプトが出した「警告:」の行。入れなかったスキルと、その理由が書いてある。

5. 最後に、ユーザーに `/reload-plugins` の実行を頼む。
   プラグインは、起動済みのセッションには `/reload-plugins` を実行するまで反映されない。
   これはユーザーが入力するコマンドで、シェルからは実行できない。
   スキルの置き場に入れたスキルは、Claude Code が自動で読み込む。

## 警告の読み方

- 「同じ名前の … を先に入れた」: `seiei-sogen/agent-skills` の中に同じ名前のスキルが二つある。先に見つけたほうだけを入れている。
- 「… が既にあり、このスクリプトが置いたものではない」: スキルの置き場に、同じ名前のディレクトリを別の手段（Nix の Home Manager など）で置いてある。スクリプトはそれを上書きしない。
