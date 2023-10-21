# times_ranking

## 環境
- Java17

## ビルド方法
```
mvn clean package
```

## 実行方法（ローカル）

1. Slackでアプリを作成し、ワークスペースにアプリをインストールする。  
   トークンは、botトークンでのみ動作確認しています。 トークンに必要なスコープは以下です（詳細は省略します）。
   - `channels:read`
   - `groups:read`
   - `chat:write`
2. 投稿先のチャネルに、アプリを追加する。
3. 以下のコマンドを実行する。
   ```
   SLACK_TOKEN=<トークン> java -jar target/times_ranking.jar
   ```

## 実行方法（AWS Lambda)
実行方法（ローカル）の1,2は実施済みとします。

1. AWS LambdaにJava17で関数を作成する。
2. `SLACK_TOKEN`という名前でトークンを環境変数に設定する（環境変数は暗号化する）。
3. `target/times_ranking.jar` をデプロイする。
4. 以下のフォーマットでテストパラメータを作成する（パラメータの値は適宜変更する）。
   ```json
   {
     "channelId": "Cxxxxxxxxxx",
     "channelNamePattern": ".*",
     "maxRankingCount": 10,
     "title": "title message",
     "rowPattern": "rank: $rank$, id: $id$, numOfMembers: $numOfMembers$"
   }
   ```
5. テストを実行する。
