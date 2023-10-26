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

        // チャネル一覧を取得し、ユーザ数の降順に並べる
        List<Channel> channels = activeChannels.get().stream()
            .filter(conversation -> Pattern.matches(params.channelNamePattern(), conversation.getName()))
            .map(conversation -> Channel.create(conversation.getId(), conversation.getNumOfMembers()))
            .sorted(Comparator.comparingInt(Channel::getNumOfMembers).reversed()).toList();

        // メッセージを構築する
        StringJoiner sj = new StringJoiner("\n", params.title() + "\n", "");
        long previousNumOfMembers = -1;
        int previousRankingCount = -1;
        for (int i = 0; i < channels.size(); i++) {
            Channel channelInfo = channels.get(i);
            long numOfMembers = channelInfo.getNumOfMembers();

            // 一つ前と同じユーザ数の場合は同率順位とする
            int rankingCount = previousNumOfMembers == numOfMembers ? previousRankingCount : i + 1;

            // 指定したランキングより大きくなったら抜ける
            if (rankingCount > params.maxRankingCount()) break;

            sj.add(Message.newMessage(params.rowPattern())
                    .rank(rankingCount)
                    .id(channelInfo.getId())
                    .numOfMembers(numOfMembers)
                    .build());

            previousNumOfMembers = numOfMembers;
            previousRankingCount = rankingCount;
        }

        // メッセージをチャネルに投稿する
        chatMessages.post(params.channelId(), sj.toString());
    }
}
