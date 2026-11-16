#!/usr/bin/env bash
# Claude Code のプラグインとスキルを、利用者の設定ディレクトリ（既定は ~/.claude）に入れる。
#
# 入れたものは、次に起動した Claude Code のセッションから使える。
# 起動済みのセッションに反映するには、そのセッションで /reload-plugins を実行する。
# 何度実行してもよい。入っているものは入れ直さず、スキルは最新の内容で置き換える。
#
# クラウドのセッションでは、環境のセットアップスクリプトからこのスクリプトを呼ぶ。
# セットアップスクリプトは Claude Code の起動前に走るので、最初のターンから使える。
set -euo pipefail

# 「マーケットプレイスの取得元 プラグイン名」の組。
# pstack は cursor/plugins の harness/pstack/claude-code（Claude Code 向けの生成版）が入る。
marketplace_sources_and_plugin_names=(
  "cursor/plugins pstack"
  "obra/superpowers superpowers"
  "mattpocock/skills mattpocock-skills"
  "dietrichgebert/ponytail ponytail"
)

# スキルを並べただけのリポジトリ。マーケットプレイスではないので、スキルの置き場へ複写する。
skill_collection_repository_url="https://github.com/seiei-sogen/agent-skills"
# skills/ の直下にあるグループのうち、入れないもの。
skill_collection_excluded_group_names=("anma-tsukatte-nai")

claude_skills_directory="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills"
# このスクリプトが置いたディレクトリの目印。目印のないディレクトリは、利用者のものとして触らない。
installed_marker_file_name=".installed-from-skill-collection-repository"

for marketplace_source_and_plugin_name in "${marketplace_sources_and_plugin_names[@]}"; do
  read -r marketplace_source plugin_name <<<"$marketplace_source_and_plugin_name"
  claude plugin install "$plugin_name" --marketplace "$marketplace_source" --scope user
done

skill_collection_checkout_directory="$(mktemp -d)"
trap 'rm -rf "$skill_collection_checkout_directory"' EXIT
git clone --quiet --depth 1 "$skill_collection_repository_url" "$skill_collection_checkout_directory"

excluded_group_find_arguments=()
for excluded_group_name in "${skill_collection_excluded_group_names[@]}"; do
  excluded_group_find_arguments+=(-not -path "$skill_collection_checkout_directory/skills/$excluded_group_name/*")
done

# プラグインの形をしたもの（.claude-plugin/plugin.json を持つ）は、ディレクトリごと置く。
# スキルの置き場に置いたプラグインは、Claude Code が <名前>@skills-dir として読み込む。
# それ以外は、SKILL.md を持つディレクトリを、グループの階層を外して置き場の直下に並べる。
mapfile -t plugin_shaped_directories < <(
  find "$skill_collection_checkout_directory/skills" "${excluded_group_find_arguments[@]}" \
    -path '*/.claude-plugin/plugin.json' -printf '%h\n' | xargs -r -n1 dirname | sort
)
plugin_shaped_find_arguments=()
for plugin_shaped_directory in "${plugin_shaped_directories[@]}"; do
  plugin_shaped_find_arguments+=(-not -path "$plugin_shaped_directory/*")
done
mapfile -t plain_skill_directories < <(
  find "$skill_collection_checkout_directory/skills" "${excluded_group_find_arguments[@]}" \
    "${plugin_shaped_find_arguments[@]}" -name SKILL.md -printf '%h\n' | sort
)

mkdir -p "$claude_skills_directory"
declare -A source_directory_by_installed_name=()
installed_count=0
for source_directory in "${plugin_shaped_directories[@]}" "${plain_skill_directories[@]}"; do
  installed_name="$(basename "$source_directory")"
  source_path_in_repository="${source_directory#"$skill_collection_checkout_directory"/}"
  destination_directory="$claude_skills_directory/$installed_name"

  if [[ -n "${source_directory_by_installed_name[$installed_name]:-}" ]]; then
    echo "警告: $source_path_in_repository は入れない。同じ名前の" \
      "${source_directory_by_installed_name[$installed_name]} を先に入れた。" >&2
    continue
  fi
  if [[ -e "$destination_directory" && ! -e "$destination_directory/$installed_marker_file_name" ]]; then
    echo "警告: $source_path_in_repository は入れない。" \
      "$destination_directory が既にあり、このスクリプトが置いたものではない。" >&2
    continue
  fi

  rm -rf "$destination_directory"
  cp -R "$source_directory" "$destination_directory"
  echo "$source_path_in_repository" >"$destination_directory/$installed_marker_file_name"
  source_directory_by_installed_name[$installed_name]="$source_path_in_repository"
  installed_count=$((installed_count + 1))
done

echo "$skill_collection_repository_url から $installed_count 個を $claude_skills_directory に入れた。"
