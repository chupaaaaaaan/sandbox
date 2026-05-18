package tokyo.chpn.office2text.core;

import java.util.Map;
import java.util.Objects;

public record ExtractionError(
        String sourceFile,
        String documentType,
        String stage,
        String containerName,
        String location,
        String message,
        Map<String, Object> metadata) {

    public ExtractionError {
        Objects.requireNonNull(sourceFile, "sourceFile must not be null");
        Objects.requireNonNull(documentType, "documentType must not be null");
        Objects.requireNonNull(stage, "stage must not be null");
        Objects.requireNonNull(message, "message must not be null");
        metadata = metadata == null ? Map.of() : Map.copyOf(metadata);
    }

    public static ExtractionError of(
            String sourceFile,
            DocumentType documentType,
            String stage,
            String containerName,
            String location,
            String message,
            Map<String, Object> metadata) {
        return new ExtractionError(
                sourceFile,
                documentType.jsonValue(),
                stage,
                containerName,
                location,
                message,
                metadata);
    }

    public static ExtractionError unsupportedFileType(String sourceFile) {
        return new ExtractionError(
                sourceFile,
                "unknown",
                Stages.DETECT_FORMAT,
                null,
                null,
                "Unsupported file type",
                Map.of());
    }
}
