#!/bin/bash

### あるブランチがマージ可能かどうかを判定し、状態をCSV形式で出力するスクリプト。
### コンフリクト発生/マージ可能/すでに最新のいずれかのステータスを出力する。
### コンフリクトが発生している場合は、コンフリクトメッセージのみ抜き出して出力する。

### 引数
### - カレントディレクトリから見た対象リポジトリへのパス
### - マージ先のブランチ
### - マージ元のブランチ

### ~git merge~ コマンドの実行結果から、マージ可能かどうかを以下のように判定することができる。
### - 最終行に ${CONFLICT_MESSAGE} が出力されていた場合 :: コンフリクトが発生している。マージする際には手動で修正が必要。
### - 最終行に ${MERGABLE_MESSAGE} が出力されていた場合 :: そのままマージできる。
### - 最終行に ${UPTODATE_MESSAGE} が出力されていた場合 :: すでに最新状態である。

### 出力形式はCSVで、以下のカラムとなる（各カラムは""で囲まれる）。
### REPO_PATH MERGE_TO MERGE_FROM MERGE_STATUS CONFLICT_MESSAGE
### - REPO_PATH、MERGE_TO、MERGE_FROMはそれぞれ、入力で与えた値である。
### - MERGE_STATUSは以下の3つのいずれかである。
###   - C :: コンフリクト発生
###   - M :: マージ可能
###   - U :: すでに最新
### - CONFLICT_MESSAGEは、出力されたコンフリクトメッセージである。
###   コンフリクトメッセージは存在しない場合もあれば、複数行に渡る場合もある。

set -eu

repo=${1:?You must specify the path of the repo.}
shift
merge_to=${1:?You must specify MERGE_TO branch.}
shift
merge_from=${1:?You must specify MERGE_FROM branch.}

readonly MERGE_INFO_TEMP="/tmp/merge_info_temp.txt"

rm -f "${MERGE_INFO_TEMP}"

git -C "${repo}" stash -u > /dev/null 2>&1 &&
    git -C "${repo}" checkout "${merge_to}" > /dev/null 2>&1 &&
    (git -C "${repo}" merge --no-commit --no-ff "${merge_from}" || exit 0) > "${MERGE_INFO_TEMP}" 2>&1

git -C "${repo}" reset --hard HEAD > /dev/null 2>&1

last_message="$(tail -1 ${MERGE_INFO_TEMP})"

readonly CONFLICT_MESSAGE="Automatic merge failed; fix conflicts and then commit the result."
readonly MERGABLE_MESSAGE="Automatic merge went well; stopped before committing as requested"
readonly UPTODATE_MESSAGE="Already up to date."

status="";

case "${last_message}" in
    "${CONFLICT_MESSAGE}")
        status="C"
        ;;
    "${MERGABLE_MESSAGE}")
        status="M"
        ;;
    "${UPTODATE_MESSAGE}")
        status="U"
        ;;
    *)
        echo "Unexpected state. The last message is '${last_message}'." 2>&1
        exit 1
        ;;
esac

repo="\"$(echo ${repo}|sed 's/"/"""/g')\""
merge_to="\"$(echo ${merge_to}|sed 's/"/"""/g')\""
merge_from="\"$(echo ${merge_from}|sed 's/"/"""/g')\""
status="\"${status}\""
conflicts="\"$(cat ${MERGE_INFO_TEMP}|grep '^CONFLICT'|sed 's/"/"""/g')\""

echo "${repo},${merge_to},${merge_from},${status},${conflicts}"
