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
public class ActiveChannels {

    private final Iterable<Conversation> iterable;

    public ActiveChannels(String token) {
        this.iterable = () -> new ActiveChannelIterator(token);
    }

    /**
     * Slackチャネルを取得する。
     * @return チャネルのStream
     */
    public Stream<Conversation> get() {
        return StreamSupport.stream(iterable.spliterator(), false);
    }

    private static class ActiveChannelIterator implements Iterator<Conversation> {

        private final Slack slack = Slack.getInstance();

        private final String token;

        private final boolean excludeB;

        private final int limitCount;

        private String nextCursor;

        private Iterator<Conversation> it;

        public ActiveChannelIterator(String token) {
            this(token, 100, true);
        }

        public ActiveChannelIterator(String token, int limitCount, boolean excludeB) {
            this.token = token;
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
                throw new IllegalStateException("Invalid status: nextCursor is not empty and conservations is empty.");
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
}
