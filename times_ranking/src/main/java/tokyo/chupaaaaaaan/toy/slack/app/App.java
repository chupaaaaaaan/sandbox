package tokyo.chupaaaaaaan.toy.slack.app;

import tokyo.chupaaaaaaan.toy.slack.client.ActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.ChatMessages;
import tokyo.chupaaaaaaan.toy.slack.model.Channel;
import tokyo.chupaaaaaaan.toy.slack.model.Message;

import java.util.Comparator;
import java.util.List;
import java.util.StringJoiner;
import java.util.regex.Pattern;

/**
 * アプリケーションのメイン処理。
 * <p>
 * 注意：このアプリケーションは基本的なアプリの設定を利用しており、
 * アプリをインストールしたワークスペースのみで使用することを想定している。
 * そのため、情報は当該ワークスペースからのみ取得され、投稿は当該ワークスペース向けに限られる。
 */
public class App {

    private final ActiveChannels activeChannels;
    private final ChatMessages chatMessages;

    public App(ActiveChannels activeChannels,
               ChatMessages chatMessages) {
        this.activeChannels = activeChannels;
        this.chatMessages = chatMessages;
    }

    public void execute(AppParams params) {

        String title = params.title();
        StringJoiner sj = new StringJoiner("\n", title + "\n", "");

        // チャネル一覧を取得し、ユーザ数の降順に並べる
        String channelNamePattern = params.channelNamePattern();
        long maxRankingCount = params.maxRankingCount();
        List<Channel> channels = activeChannels.get().stream()
            .filter(conversation -> Pattern.matches(channelNamePattern, conversation.getName()))
            .map(conversation -> Channel.create(conversation.getId(), conversation.getNumOfMembers()))
            .sorted(Comparator.comparingInt(Channel::getNumOfMembers).reversed())
            .limit(maxRankingCount).toList();

        // メッセージを構築する
        String rowPattern = params.rowPattern();
        for (int i = 0; i < channels.size(); i++) {
            sj.add(Message.newMessage(rowPattern)
                    .rank(i+1)
                    .id(channels.get(i).getId())
                    .numOfMembers(channels.get(i).getNumOfMembers())
                    .build());
        }

        // メッセージをチャネルに投稿する
        String channelId = params.channelId();
        chatMessages.post(channelId, sj.toString());
    }
}
