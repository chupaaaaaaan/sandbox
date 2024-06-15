package tokyo.chpn.ibmmq.sampleclient;

import java.io.IOException;

import com.ibm.mq.MQException;
import com.ibm.mq.MQGetMessageOptions;
import com.ibm.mq.MQMessage;
import com.ibm.mq.MQQueue;
import com.ibm.mq.MQQueueManager;
import com.ibm.mq.constants.CMQC;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) {

        Option s = Option.builder("s").longOpt("send")   .build();
        Option r = Option.builder("r").longOpt("receive").build();

        OptionGroup optionGroup = new OptionGroup();
        optionGroup.addOption(s);
        optionGroup.addOption(r);
        optionGroup.setRequired(true);

        Option qm = Option.builder("qm").required().hasArg().longOpt("qmanager").build();
        Option q  = Option.builder("q") .required().hasArg().longOpt("queue")   .build();
        Option m  = Option.builder("m")            .hasArg().longOpt("message") .build();

        Options options = new Options();
        options.addOptionGroup(optionGroup);
        options.addOption(qm);
        options.addOption(q);
        options.addOption(m);

        CommandLineParser parser = new DefaultParser();

        try {
            CommandLine cmd = parser.parse(options, args);
            // キュー・マネージャーと接続
            MQQueueManager queueManager = new MQQueueManager(cmd.getOptionValue(qm));

            String queueName = cmd.getOptionValue(q);
            if (cmd.hasOption(s)) {
                // 送信モードの処理
                // キューに接続
                MQQueue queue = queueManager.accessQueue(queueName, CMQC.MQOO_OUTPUT);

                // メッセージを作成
                String message = cmd.hasOption(m) ? cmd.getOptionValue(m) : "Hello, IBM MQ!";
                MQMessage mqMessage = new MQMessage();
                mqMessage.writeString(message);

                // メッセージをキューに送信
                queue.put(mqMessage);
                queue.close();

                System.out.println("Sent message:");
                System.out.println(message);

            } else if (cmd.hasOption(r)) {
                // 受信モードの処理
                // キューに接続
                MQQueue queue = queueManager.accessQueue(queueName, CMQC.MQOO_INPUT_AS_Q_DEF);

                // 受信メッセージを作成
                MQMessage mqMessage = new MQMessage();
                MQGetMessageOptions gmo = new MQGetMessageOptions();

                // メッセージをキューから受信
                queue.get(mqMessage, gmo);
                queue.close();

                String receivedMessage = mqMessage.readStringOfByteLength(mqMessage.getDataLength());

                System.out.println("Received message:");
                System.out.println(receivedMessage);

            } else {
                throw new RuntimeException("Not come here.");
            }

            queueManager.disconnect();

        } catch (ParseException | MQException | IOException e) {
            throw new RuntimeException(e);
        }

    }
}
