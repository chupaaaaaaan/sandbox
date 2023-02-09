package tokyo.chupaaaaaaan.slack;

import tokyo.chupaaaaaaan.slack.client.GetAllActiveChannels;
import tokyo.chupaaaaaaan.slack.model.BasicChannel;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Hello world!
 */
public class App {
    public static void main(String[] args) {

        String token = System.getenv("SLACK_TOKEN");

        List<BasicChannel> basicChannels = new GetAllActiveChannels().execute(token).stream()
            .map(c -> new BasicChannel(c.getId(), c.getName(), c.getNumOfMembers()))
            .sorted()
            .collect(Collectors.toUnmodifiableList());

        basicChannels.forEach(System.out::println);


    }
}
