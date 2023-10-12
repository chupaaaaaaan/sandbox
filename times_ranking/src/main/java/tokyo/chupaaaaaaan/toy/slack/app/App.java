package tokyo.chupaaaaaaan.toy.slack.app;

import tokyo.chupaaaaaaan.toy.slack.client.ActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.ChatMessages;
import tokyo.chupaaaaaaan.toy.slack.model.Channel;

import java.util.Comparator;
import java.util.StringJoiner;
import java.util.regex.Pattern;

/**
 * アプリケーション実行の起点となるクラス。
 * 小さいアプリであるため、アプリのロジックもこのクラスに記載している。
 * Slackへの接続を伴う処理を実行するクラスはclientパッケージへ切り出している。
 * 注意：このアプリケーションは基本的なアプリの設定を利用しており、
 * アプリをインストールしたワークスペースのみで使用することを想定している。
 * そのため、情報は当該ワークスペースのもののみ取得・投稿は当該ワークスペースに限られる。
 */
public class App {

    private final ActiveChannels activeChannels;
    private final ChatMessages chatMessages;
    private final String channelId;
    private final String channelNamePattern;
    private final long maxRankingCount;

    public App(ActiveChannels activeChannels,
               ChatMessages chatMessages,
               String channelId,
               String channelNamePattern,
               long maxRankingCount) {
        this.activeChannels = activeChannels;
        this.chatMessages = chatMessages;
        this.channelId = channelId;
        this.channelNamePattern = channelNamePattern;
        this.maxRankingCount = maxRankingCount;
    }

    public void execute() {

        StringJoiner sj = new StringJoiner("\n", "今日のtimesユーザ数ランキングはこちら！\n", "");

        // チャネル一覧を取得し、ユーザ数の降順に並べ、順に投稿メッセージを構築する
        activeChannels.get()
            .filter(conversation -> Pattern.matches(channelNamePattern, conversation.getName()))
            .map(conversation -> Channel.create(conversation.getId(), conversation.getNumOfMembers()))
            .sorted(Comparator.comparingInt(Channel::getNumOfMembers).reversed())
            .limit(maxRankingCount)
            .forEach(channel -> sj.add(channel.toMessage()));

        // メッセージをチャネルに投稿する
        chatMessages.post(channelId, sj.toString());
    }
}
