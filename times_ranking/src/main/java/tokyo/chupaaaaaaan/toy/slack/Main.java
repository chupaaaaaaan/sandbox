package tokyo.chupaaaaaaan.toy.slack;

import tokyo.chupaaaaaaan.toy.slack.app.App;
import tokyo.chupaaaaaaan.toy.slack.client.ActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.ChatMessages;

public class Main {

    public static void main(String[] args) {
        String token = System.getenv("SLACK_TOKEN");
        ActiveChannels activeChannels = new ActiveChannels(token);
        ChatMessages chatMessages = new ChatMessages(token);

        App app = new App(activeChannels, chatMessages, "C010M580R16", "^a.*", 10);

        app.execute();
    }
}
