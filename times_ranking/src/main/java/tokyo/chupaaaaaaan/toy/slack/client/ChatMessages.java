package tokyo.chupaaaaaaan.toy.slack.client;

import com.slack.api.Slack;
import com.slack.api.methods.SlackApiException;
import com.slack.api.methods.request.chat.ChatPostMessageRequest;
import com.slack.api.methods.response.chat.ChatPostMessageResponse;

import java.io.IOException;

/**
 * Slackのチャネルにメッセージを投稿するSlackクライアント
 */
public class ChatMessages {

    private final String token;

    public ChatMessages(String token) {
        this.token = token;
    }

    /**
     * メッセージをチャンネルに投稿する。
     * @param channel 投稿先チャネル
     * @param message 投稿するメッセージ
     */
    public void post(String channel, String message) {

        final Slack slack = Slack.getInstance();

        ChatPostMessageRequest request = ChatPostMessageRequest.builder()
            .channel(channel)
            .text(message)
            .build();

        try {
            ChatPostMessageResponse response = slack.methods(token).chatPostMessage(request);
            if (!response.isOk()) {
                throw new IllegalStateException(response.getError());
            }
        } catch (SlackApiException | IOException e) {
            throw new IllegalStateException(e);
        }
    }
}
