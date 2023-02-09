package tokyo.chupaaaaaaan.toy.slack.client;

import com.slack.api.Slack;
import com.slack.api.methods.SlackApiException;
import com.slack.api.methods.request.chat.ChatPostMessageRequest;
import com.slack.api.methods.response.chat.ChatPostMessageResponse;

import java.io.IOException;

/**
 * Slackのチャネルにメッセージを投稿するためのクラス
 */
public class PostMessages {

    private final Slack slack = Slack.getInstance();

    /**
     * メッセージをチャンネルに投稿する。
     * @param token Slackトークン
     * @param channel 投稿先チャネル
     * @param message 投稿するメッセージ
     */
    public void execute(String token, String channel, String message) {
        ChatPostMessageRequest request = ChatPostMessageRequest.builder()
            .channel(channel)
            .text(message)
            .build();

        try {
            ChatPostMessageResponse response = slack.methods(token).chatPostMessage(request);
            if (!response.isOk()) {
                throw new RuntimeException(response.getError());
            }
        } catch (SlackApiException | IOException e) {
            throw new RuntimeException(e);
        }
    }
}
