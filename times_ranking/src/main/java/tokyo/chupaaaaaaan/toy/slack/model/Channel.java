package tokyo.chupaaaaaaan.toy.slack.model;

/**
 * アプリで使用するチャネル情報を格納するクラス。
 */
public class Channel {

    private final String id;

    private final int numOfMembers;

    private Channel(String id, int numOfMembers) {
        this.id = id;
        this.numOfMembers = numOfMembers;
    }

    public String getId() {
        return id;
    }

    public int getNumOfMembers() {
        return numOfMembers;
    }

    public static Builder newChannel(String id, int numOfMembers) {
        return new Builder(id, numOfMembers);
    }

    public static Channel create (String id, int numOfMembers) {
        return newChannel(id, numOfMembers).build();
    }

    public static class Builder {

        private final Channel channel;

        private Builder(String id, int numOfMembers) {
            channel = new Channel(id, numOfMembers);
        }

        public Channel build() {
            return channel;
        }
    }
}
