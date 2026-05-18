package tokyo.chpn.office2text.io;

import tools.jackson.core.StreamWriteFeature;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.PropertyNamingStrategies;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

public final class ObjectMapperFactory {

    private static final ObjectMapper INSTANCE = createInternal();

    private ObjectMapperFactory() {
    }

    public static ObjectMapper getInstance() {
        return INSTANCE;
    }

    private static ObjectMapper createInternal() {
        return JsonMapper.builder()
                .disable(SerializationFeature.INDENT_OUTPUT)
                .disable(StreamWriteFeature.AUTO_CLOSE_TARGET)
                .propertyNamingStrategy(PropertyNamingStrategies.SNAKE_CASE)
                .build();
    }
}
