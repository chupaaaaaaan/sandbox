package tokyo.chpn.office2text.core;

import org.junit.jupiter.api.Test;
import tokyo.chpn.office2text.io.ObjectMapperFactory;
import tools.jackson.core.StreamWriteFeature;
import tools.jackson.databind.*;
import tools.jackson.databind.json.JsonMapper;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class ExtractedTextTest {

    @Test
    void extractedText_isSerializedWithExpectedFields() throws Exception {
        ObjectMapper mapper = ObjectMapperFactory.getInstance();

        ExtractedText dto = ExtractedText.of(
                "sample.xlsx",
                DocumentType.XLSX,
                PartTypes.CELL,
                "Sheet1",
                "A1",
                "hello",
                null
        );

        String json = mapper.writeValueAsString(dto);
        System.out.println(json);
        JsonNode node = mapper.readTree(json);

        assertEquals("sample.xlsx", node.get("source_file").asString());
        assertEquals("xlsx", node.get("document_type").asString());
        assertEquals("cell", node.get("part_type").asString());
        assertEquals("Sheet1", node.get("container_name").asString());
        assertEquals("A1", node.get("location").asString());
        assertEquals("hello", node.get("text").asString());

        assertTrue(node.has("metadata"));
        assertTrue(node.get("metadata").isObject());
        assertEquals(0, node.get("metadata").size());
    }

    @Test
    void extractedText_serializesNullFieldsAsJsonNull() throws Exception {
        ObjectMapper mapper = ObjectMapperFactory.getInstance();

        ExtractedText dto = new ExtractedText(
                "sample.xlsx",
                "xlsx",
                "cell",
                null,
                null,
                "hello",
                null
        );

        String json = mapper.writeValueAsString(dto);
        JsonNode node = mapper.readTree(json);

        assertTrue(node.has("container_name"));
        assertTrue(node.get("container_name").isNull());

        assertTrue(node.has("location"));
        assertTrue(node.get("location").isNull());

        assertTrue(node.has("metadata"));
        assertTrue(node.get("metadata").isObject());
    }

    @Test
    void of_createsExpectedObject() {
        ExtractedText dto = ExtractedText.of(
                "sample.xlsx",
                DocumentType.XLSX,
                PartTypes.CELL,
                "Sheet1",
                "A1",
                "hello",
                null
        );

        assertEquals("sample.xlsx", dto.sourceFile());
        assertEquals("xlsx", dto.documentType());
        assertEquals("cell", dto.partType());
        assertEquals("Sheet1", dto.containerName());
        assertEquals("A1", dto.location());
        assertEquals("hello", dto.text());

        // metadata の扱い（仕様次第）
        assertNotNull(dto.metadata());
        assertTrue(dto.metadata().isEmpty());
    }


}