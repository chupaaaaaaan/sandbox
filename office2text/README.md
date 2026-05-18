# office2text

Office ファイルから文字列を抽出し、JSON Lines 形式で出力する CLI ツール。

本ツールは「抽出専用」に特化しており、フィルタリングは行わない。  
抽出結果は `jq`, `grep`, `rg` 等の外部ツールで処理することを前提とする。

---

## 特徴

- Office ファイルからテキストを抽出
- JSON Lines (1 行 1 レコード) で出力
- stdout に正常系、stderr にエラーを出力
- 空文字・空白のみの要素は出力しない
- シンプルでパイプ処理に適した設計

---

## 対応状況（フェーズ1）

### 対応形式

- `.xlsx`
- `.xlsm`

### 抽出対象

- セル (`cell`) の表示文字列

### 未実装（今後対応予定）

- Excel コメント (`comment`)
- Excel 図形 (`shape`)
- Word (`.docx`)

---

## ビルド

Maven を使用

    mvn package

---

## 実行

exec-maven-plugin を使用する場合

    mvn exec:java -Dexec.args="sample.xlsx"

---

## 出力形式

### 正常系（stdout）

1 行につき 1 JSON オブジェクト

例:

    {"source_file":"sample.xlsx","document_type":"xlsx","part_type":"cell","container_name":"Sheet1","location":"A1","text":"hello","metadata":{}}

---

### エラー（stderr）

例:

    {"source_file":"sample.txt","document_type":"unknown","stage":"detect-format","container_name":null,"location":null,"message":"Unsupported file type","metadata":{}}

---

## 終了コード

- 0: 成功（エラーなし）
- 1: 抽出中にエラーあり
- 2: 引数不正

---

## 制約

- 単一ファイル入力のみ
- JSON Lines 固定（pretty print なし）
- 空白のみの要素は出力しない

---

## 使用例

xlsx から抽出して grep

    mvn exec:java -Dexec.args="sample.xlsx" | grep hello

jq で整形

    mvn exec:java -Dexec.args="sample.xlsx" | jq .

---

## 今後の予定

- Excel コメント抽出
- Excel 図形抽出
- Word 対応
- ディレクトリ一括処理
- 入力形式の拡張

---

## ライセンス

TBD