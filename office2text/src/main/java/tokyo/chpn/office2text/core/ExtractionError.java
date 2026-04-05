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
        String cause,
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
            Throwable cause,
            Map<String, Object> metadata) {
        return new ExtractionError(
                sourceFile,
                documentType.jsonValue(),
                stage,
                containerName,
                location,
                message,
                cause == null ? null : cause.getClass().getName(),
                metadata);
    }

    public static ExtractionError unsupportedFileType(String sourceFile) {
        return new ExtractionError(
                sourceFile,
                "unknown",
                "detect-format",
                null,
                null,
                "Unsupported file type",
                null,
                Map.of());
    }
}
