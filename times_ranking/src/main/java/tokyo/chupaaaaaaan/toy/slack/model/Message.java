package tokyo.chupaaaaaaan.toy.slack.model;

/**
 * 投稿するメッセージのビルダークラス。
 */
public class Message {

    private String message;

    public static Message newMessage(String message) {
        return new Message(message);
    }

    private Message(String message) {
        this.message = message;
    }

    public Message rank(int val) {
        message = message.replace("$rank$", String.valueOf(val));
        return this;
    }

    public Message id(String val) {
        message = message.replace("$id$", "<#" + val + ">");
        return this;
    }

    public Message numOfMembers(int val) {
        message = message.replace("$numOfMembers$", String.valueOf(val));
        return this;
    }

    public String build() {
        return message;
    }
}
