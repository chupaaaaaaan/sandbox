package tokyo.chupaaaaaaan.toy.slack.client;

import com.slack.api.Slack;
import com.slack.api.methods.SlackApiException;
import com.slack.api.methods.request.conversations.ConversationsListRequest;
import com.slack.api.methods.response.conversations.ConversationsListResponse;
import com.slack.api.model.Conversation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * チャネル一覧を{@link List}形式で取得するSlackクライアント。
 */
public class ActiveChannels {

    private final String token;

    public ActiveChannels(String token) {
        this.token = token;
    }

    /**
     * Slackチャネルを取得する。
     * @return チャネルのList
     */
    public List<Conversation> get() {
        return getAllConversation();
    }

    private List<Conversation> getAllConversation() {
        List<Conversation> result = new ArrayList<>();
        String nextCursor = "";

        do {
            ConversationsListResponse response = execRequest(request(nextCursor));
            result.addAll(response.getChannels());
            nextCursor = response.getResponseMetadata().getNextCursor();
        } while (!nextCursor.isEmpty());

        return Collections.unmodifiableList(result);
    }

    private ConversationsListRequest request(String nextCursor) {
        ConversationsListRequest.ConversationsListRequestBuilder builder =
                ConversationsListRequest.builder().excludeArchived(true).limit(100);
        if (!nextCursor.isEmpty()) {
            builder = builder.cursor(nextCursor);
        }
        return builder.build();
    }

    private ConversationsListResponse execRequest(ConversationsListRequest request) {
        final Slack slack = Slack.getInstance();

        try {
            ConversationsListResponse response = slack.methods(token).conversationsList(request);
            if (response.isOk()) {
                return response;
            } else {
                throw new IllegalStateException(response.getError());
            }
        } catch (SlackApiException | IOException e) {
            throw new IllegalStateException(e);
        }
    }
}
