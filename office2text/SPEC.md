# office2text 仕様書（フェーズ1）

---

## 1. 目的

Office ファイルからテキストを抽出し、構造を持った JSON Lines として出力する。

本ツールは抽出のみを担当し、検索・フィルタリングは外部ツールに委ねる。

---

## 2. 対象ファイル

### 対応

- xlsx
- xlsm

### 非対応

- txt
- csv
- その他すべての非 Office ファイル

非対応ファイルはエラーとする。

---

## 3. 出力仕様

### 3.1 形式

- JSON Lines
- 1 行 = 1 レコード
- pretty print なし

---

### 3.2 正常系 DTO

フィールド:

- `source_file`
- `document_type`
- `part_type`
- `container_name`
- `location`
- `text`
- `metadata`

例:

    {"source_file":"sample.xlsx","document_type":"xlsx","part_type":"cell","container_name":"Sheet1","location":"A1","text":"hello","metadata":{}}

---

### 3.3 エラー DTO

フィールド:

- `source_file`
- `document_type`
- `stage`
- `container_name`
- `location`
- `message`
- `metadata`

例:

    {"source_file":"sample.xlsx","document_type":"xlsx","stage":"extract-cell","container_name":"Sheet1","location":"A1","message":"Failed to extract cell.","metadata":{}}

---

### 3.4 JSON 方針

- キー順序は固定
- `null` は省略せず出力
- `metadata` は必ず出力（空の場合は `{}`）

---

## 4. `part_type`

### 現在

- `cell`

### 将来予定

- `comment`
- `shape`
- `paragraph`
- `table_cell`

---

## 5. `stage`

### 共通

- `detect-format`

### Excel

- `load-workbook`
- `extract-cell`

（`comment` / `shape` は将来追加）

---

## 6. `location`

### Excel

- セル: `A1`

将来:

- コメント: `comment:B2`
- 図形: `shape:1`
- グループ図形: `shape:1/child:2`

---

## 7. `container_name`

- Excel: シート名

---

## 8. `text`

- 表示文字列を出力
  - 数式は計算結果を出力
- 空文字または空白のみの場合は出力しない
- trim は行わない

---

## 9. `metadata`

- 常に出力
- フェーズ1では空 object

将来:

- `comment`: `author`
- `shape`: `object_type`, `object_name`, `object_id`
- `cell`: `formula` 等

---

## 10. フォーマット判定

- 拡張子ベース
- xlsx / xlsm のみ対応

将来:
- OOXML 内容確認

---

## 11. 抽出仕様（フェーズ1）

### Excel

#### セル

- 行 → 列 の順で走査
- `DataFormatter` + `FormulaEvaluator` を使用
- 表示文字列を抽出
- blank はスキップ

---

## 12. エラー処理

### 抽出エラー

- 標準エラーに出力
- 処理は継続

### I/O エラー

- `IOException` として上位に伝播
- ツールの実行失敗として扱う

---

## 13. 終了コード

- `0`: エラーなし
- `1`: エラーあり
- `2`: 引数不正

---

## 14. CLI 仕様

- 入力は単一ファイルのみ
- ディレクトリは未対応

---

## 15. 実装方針

- 抽出は逐次処理（streaming）
- Consumer（RecordSink）に流す設計
- stdout: 正常系
- stderr: エラー

---

## 16. 将来拡張

- Excel comment
- Excel shape
- Word 対応
- 複数ファイル入力
- ディレクトリ再帰
- 入力フォーマット拡張