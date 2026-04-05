package tokyo.chpn.office2text.io;

import java.io.IOException;
import java.io.Writer;

import tools.jackson.core.StreamWriteFeature;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

public final class JacksonJsonLineWriter<T> {
    private final ObjectMapper objectMapper;
    private final Writer writer;


    public JacksonJsonLineWriter(ObjectMapper objectMapper, Writer writer) {
        this.objectMapper = objectMapper;
        this.writer = writer;
    }

    public void write(T value) throws IOException {
        objectMapper.writeValue(writer, value);
        writer.write(System.lineSeparator());
        writer.flush();
    }
}