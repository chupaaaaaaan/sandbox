package tokyo.chupaaaaaaan.toy.slack;

import com.amazonaws.services.kms.AWSKMS;
import com.amazonaws.services.kms.AWSKMSClientBuilder;
import com.amazonaws.services.kms.model.DecryptRequest;
import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.LambdaLogger;
import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.amazonaws.util.Base64;
import tokyo.chupaaaaaaan.toy.slack.app.App;
import tokyo.chupaaaaaaan.toy.slack.app.AppParams;
import tokyo.chupaaaaaaan.toy.slack.client.ActiveChannels;
import tokyo.chupaaaaaaan.toy.slack.client.ChatMessages;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * AWS Lambda環境における、アプリケーション実行の起点となるクラス。
 */
public class Handler implements RequestHandler<AppParams, Void> {

    @Override
    public Void handleRequest(AppParams event, Context context)
    {
        final String token = decryptKey();

        LambdaLogger logger = context.getLogger();
        logger.log("EVENT TYPE: " + event.getClass());

        ActiveChannels activeChannels = new ActiveChannels(token);
        ChatMessages chatMessages = new ChatMessages(token);
        App app = new App(activeChannels, chatMessages);

        app.execute(event);

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
}
