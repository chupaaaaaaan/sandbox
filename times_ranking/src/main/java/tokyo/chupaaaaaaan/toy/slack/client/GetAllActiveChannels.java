package tokyo.chupaaaaaaan.toy.slack.client;

import com.slack.api.Slack;
import com.slack.api.methods.SlackApiException;
import com.slack.api.methods.request.conversations.ConversationsListRequest;
import com.slack.api.methods.response.conversations.ConversationsListResponse;
import com.slack.api.model.Conversation;

import java.io.IOException;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * チャネル一覧を{@link Stream}形式で取得するSlackクライアント
 */
public class GetAllActiveChannels {

    /**
     * Slackチャネルを取得する。
     * @param token Slackトークン
     * @return チャネルのStream
     */
    public static Stream<Conversation> execute(String token) {

        Iterable<Conversation> iterable = () -> new ActiveConversations(token);

        return StreamSupport.stream(iterable.spliterator(), false);
    }

    static class ActiveConversations implements Iterator<Conversation> {

        private final Slack slack = Slack.getInstance();

        private final boolean excludeB;

        private final int limitCount;

        private final String slackToken;

        private String nextCursor;

        private Iterator<Conversation> it;

        public ActiveConversations(String slackToken) {
            this(slackToken, 100, true);
        }

        public ActiveConversations(String slackToken, int limitCount, boolean excludeB) {
            this.slackToken = slackToken;
            this.limitCount = limitCount;
            this.excludeB = excludeB;
        }

        @Override
        public boolean hasNext() {
            if(it == null) {
                updateConversation(request());
            }

            // リストに要素がまだあるなら、true
            if (it.hasNext()) {
                return true;
            }

            // 要素がなく、次ページも存在しないなら、false
            if (nextCursor.isEmpty()) {
                return false;
            }

            // 上記いずれでもなければ、リストを再取得し、hasNext()
            updateConversation(request(nextCursor));
            return it.hasNext();
        }

        @Override
        public Conversation next() {
            return it.next();
        }

        private void updateConversation(ConversationsListRequest request) {
            ConversationsListResponse response = execRequest(request);
            it = response.getChannels().iterator();
            nextCursor = response.getResponseMetadata().getNextCursor();
            if (!it.hasNext() && !nextCursor.isEmpty()) {
                throw new RuntimeException("Invalid status: nextCursor is not empty and conservations is empty.");
            }
        }

        private ConversationsListRequest request(String nc) {
            return ConversationsListRequest.builder().excludeArchived(excludeB).limit(limitCount).cursor(nc).build();
        }

        private ConversationsListRequest request() {
            return ConversationsListRequest.builder().excludeArchived(excludeB).limit(limitCount).build();
        }

        private ConversationsListResponse execRequest(ConversationsListRequest request) {
            try {
                ConversationsListResponse response = slack.methods(slackToken).conversationsList(request);
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
}
