package tokyo.chupaaaaaaan.toy.slack;

import tokyo.chupaaaaaaan.toy.slack.app.App;
import tokyo.chupaaaaaaan.toy.slack.app.AppParams;
import tokyo.chupaaaaaaan.toy.slack.client.ActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.ChatMessages;

/**
 * ローカル環境における、アプリケーション実行の起点となるクラス。
 */
public class Main {

    public static void main(String[] args) {
        String token = System.getenv("SLACK_TOKEN");
        ActiveChannels activeChannels = new ActiveChannels(token);
        ChatMessages chatMessages = new ChatMessages(token);

        AppParams params = new AppParams(
                "C010M580R16",
                "^a.*",
                10,
                "今週のtimesユーザ数ランキングはこちら！",
                "$rank$位: $id$\t 参加者数: $numOfMembers$人");
        App app = new App(activeChannels, chatMessages);

        app.execute(params);
    }
}
