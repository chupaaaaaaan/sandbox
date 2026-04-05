package tokyo.chpn.office2text.core;

import java.util.Map;
import java.util.Objects;

public record ExtractedText(
        String sourceFile,
        String documentType,
        String partType,
        String containerName,
        String location,
        String text,
        Map<String, Object> metadata) {

    public ExtractedText {
        Objects.requireNonNull(sourceFile, "sourceFile must not be null");
        Objects.requireNonNull(documentType, "documentType must not be null");
        Objects.requireNonNull(partType, "partType must not be null");
        Objects.requireNonNull(text, "text must not be null");
        metadata = metadata == null ? Map.of() : Map.copyOf(metadata);
    }

    public static ExtractedText of(
            String sourceFile,
            DocumentType documentType,
            String partType,
            String containerName,
            String location,
            String text,
            Map<String, Object> metadata) {
        return new ExtractedText(
                sourceFile,
                documentType.jsonValue(),
                partType,
                containerName,
                location,
                text,
                metadata);
    }

}
