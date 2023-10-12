package tokyo.chupaaaaaaan.toy.slack;

import com.amazonaws.services.kms.AWSKMS;
import com.amazonaws.services.kms.AWSKMSClientBuilder;
import com.amazonaws.services.kms.model.DecryptRequest;
import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.LambdaLogger;
import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.amazonaws.util.Base64;
import tokyo.chupaaaaaaan.toy.slack.app.App;
import tokyo.chupaaaaaaan.toy.slack.client.ActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.ChatMessages;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public class Handler implements RequestHandler<Handler.AppParams, Void> {

    private static final String token = decryptKey();

    @Override
    public Void handleRequest(AppParams event, Context context)
    {
        LambdaLogger logger = context.getLogger();
        logger.log("EVENT TYPE: " + event.getClass());

        ActiveChannels activeChannels = new ActiveChannels(token);
        ChatMessages chatMessages = new ChatMessages(token);

        String channelId = event.channelId();
        String channelNamePrefix = event.channelNamePattern();
        long maxRankingCount = event.maxRankingCount();

        App app = new App(activeChannels, chatMessages, channelId, channelNamePrefix, maxRankingCount);

        app.execute();

        return null;
    }

    private static String decryptKey() {
        byte[] encryptedKey = Base64.decode(System.getenv("SLACK_TOKEN"));
        Map<String, String> encryptionContext = new HashMap<>();
        encryptionContext.put("LambdaFunctionName",
                System.getenv("AWS_LAMBDA_FUNCTION_NAME"));

        AWSKMS client = AWSKMSClientBuilder.defaultClient();

        DecryptRequest request = new DecryptRequest()
                .withCiphertextBlob(ByteBuffer.wrap(encryptedKey))
                .withEncryptionContext(encryptionContext);

        ByteBuffer plainTextKey = client.decrypt(request).getPlaintext();
        return new String(plainTextKey.array(), StandardCharsets.UTF_8);
    }

    public record AppParams(String channelId, String channelNamePattern, long maxRankingCount) {}
}
