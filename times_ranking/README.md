# times_ranking

## ビルド

```
mvn clean compile
```

## 実行方法

1. Slackでアプリを作成し、ワークスペースにアプリをインストールする。  
トークンは、botトークンでのみ動作確認しています。
トークンに必要なスコープは以下です。

- `channels:read`
- `groups:read`
- `chat:write`

詳細は省略します。

2. 投稿先のチャネルに、アプリを追加する。
3. 以下のコマンドを実行する。

```
SLACK_CHANNEL=<投稿先のChannel ID> SLACK_TOKEN=<トークン> mvn exec:java -Dexec.cleanupDaemonThreads=false -Dexec.mainClass=tokyo.chupaaaaaaan.toy.slack.App
```

## 残作業

- [ ] JARパッケージにする
- [ ] どこかのプラットフォームにデプロイする手順を整える
