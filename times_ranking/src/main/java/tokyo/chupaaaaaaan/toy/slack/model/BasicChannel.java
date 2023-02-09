package tokyo.chupaaaaaaan.toy.slack.model;

/**
 * アプリで使用するチャネル情報を格納するクラス
 */
public class BasicChannel {

    private final String id;

    private final int numOfMembers;

    public BasicChannel(String id, int numOfMembers) {
        this.id = id;
        this.numOfMembers = numOfMembers;
    }

    public String getId() {
        return id;
    }

    public int getNumOfMembers() {
        return numOfMembers;
    }

    @Override
    public String toString() {
        return "<#" + getId() + ">\t: " + getNumOfMembers();
    }
}
