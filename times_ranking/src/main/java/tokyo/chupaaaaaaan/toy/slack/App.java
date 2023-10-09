package tokyo.chupaaaaaaan.toy.slack;

import tokyo.chupaaaaaaan.toy.slack.client.GetAllActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.PostMessages;
import tokyo.chupaaaaaaan.toy.slack.model.Channel;

import java.util.Comparator;
import java.util.StringJoiner;

/**
 * アプリケーション実行の起点となるクラス。
 * 小さいアプリであるため、アプリのロジックもこのクラスに記載している。
 * Slackへの接続を伴う処理を実行するクラスはclientパッケージへ切り出している。
 * 注意：このアプリケーションは基本的なアプリの設定を利用しており、
 * アプリをインストールしたワークスペースのみで使用することを想定している。
 * そのため、情報は当該ワークスペースのもののみ取得・投稿は当該ワークスペースに限られる。
 */
public class App {
    public static void main(String[] args) {

        String token = System.getenv("SLACK_TOKEN");
        String channel = System.getenv("SLACK_CHANNEL");

        String channelNamePrefix = "a";
        long maxRankingCount = 10;

        StringJoiner sj = new StringJoiner("\n", "今日のtimesユーザ数ランキングはこちら！\n", "");

        // チャネル一覧を取得し、ユーザ数の降順に並べ、順に投稿メッセージを構築する
        GetAllActiveChannels.execute(token)
            .filter(c -> c.getName().startsWith(channelNamePrefix))
            .map(c -> Channel.create(c.getId(), c.getNumOfMembers()))
            .sorted(Comparator.comparingInt(Channel::getNumOfMembers).reversed())
            .limit(maxRankingCount)
            .forEach(c -> sj.add(c.toMessage()));

        // メッセージをチャネルに投稿する
        PostMessages.execute(token, channel, sj.toString());
    }
}
