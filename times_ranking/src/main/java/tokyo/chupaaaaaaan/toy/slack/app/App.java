package tokyo.chupaaaaaaan.toy.slack.app;

import com.slack.api.model.Conversation;
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
        List<Channel> channels = activeChannels.get().stream()
            .filter(conversation -> Pattern.matches(channelNamePattern, conversation.getName()))
            .map(conversation -> Channel.create(conversation.getId(), conversation.getNumOfMembers()))
            .sorted(Comparator.comparingInt(Channel::getNumOfMembers).reversed()).toList();

        // メッセージを構築する
        String rowPattern = params.rowPattern();
        long maxRankingCount = params.maxRankingCount();

        long previousCount = -1;
        int previousRank = -1;
        for (int i = 0; i < channels.size(); i++) {
            Channel channelInfo = channels.get(i);
            long currentCount = channelInfo.getNumOfMembers();

            // 一つ前と同じユーザ数の場合は同率順位とする
            int currentRank = previousCount == currentCount ? previousRank : i + 1;

            // 指定したランキングより大きくなったら抜ける
            if (currentRank > maxRankingCount) break;

            sj.add(Message.newMessage(rowPattern)
                    .rank(currentRank)
                    .id(channelInfo.getId())
                    .numOfMembers(currentCount)
                    .build());

            previousCount = currentCount;
            previousRank = currentRank;
        }

        // メッセージをチャネルに投稿する
        String channelId = params.channelId();
        chatMessages.post(channelId, sj.toString());
    }
}
