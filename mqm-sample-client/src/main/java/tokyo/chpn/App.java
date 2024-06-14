package tokyo.chpn;

import java.io.IOException;

import com.ibm.mq.MQEnvironment;
import com.ibm.mq.MQException;
import com.ibm.mq.MQMessage;
import com.ibm.mq.MQQueue;
import com.ibm.mq.MQQueueManager;
import com.ibm.mq.constants.CMQC;

import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) {

        Option queueManagerName1 = Option.builder("m").hasArg().required().numberOfArgs(1).optionalArg(false).longOpt("qmanager").build();
        Option queueName1        = Option.builder("q").hasArg().required().numberOfArgs(1).optionalArg(false).longOpt("queue").build();
        Option channel1          = Option.builder("c").hasArg().required().numberOfArgs(1).optionalArg(false).longOpt("channel").build();
        Option host1             = Option.builder("h").hasArg().required().numberOfArgs(1).optionalArg(false).longOpt("host").build();
        Option port1             = Option.builder("p").hasArg().required().numberOfArgs(1).optionalArg(false).longOpt("port").type(Integer.class).build();

        Options options = new Options();
        options.addOption(queueManagerName1);
        options.addOption(queueName1);
        options.addOption(channel1);
        options.addOption(host1);
        options.addOption(port1);

        CommandLineParser parser = new DefaultParser();
        
        
        try {
            // IBM MQ接続情報
            String queueManagerName = "QM1";
            String queueName = "TO.QM2";
            String host = "mq-receive";
            int port = 1415;
            String channel = "CHANNEL1";

            // MQ環境設定
            MQEnvironment.hostname = host;
            MQEnvironment.port = port;
            MQEnvironment.channel = channel;

            // キュー・マネージャーと接続
            MQQueueManager queueManager = new MQQueueManager(queueManagerName);

            // キューに接続
            int openOptions = CMQC.MQOO_OUTPUT;
            MQQueue queue = queueManager.accessQueue(queueName, openOptions);

            // メッセージを作成
            String message = "Hello, IBM MQ!";
            MQMessage mqMessage = new MQMessage();
            mqMessage.writeString(message);

            // メッセージをキューに送信
            queue.put(mqMessage);
            queue.close();
            queueManager.disconnect();

            System.out.println("メッセージが送信されました");
        } catch (MQException | IOException e) {
            throw new RuntimeException(e);
        }

    }
}
