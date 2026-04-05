package tokyo.chpn.office2text.core;

import java.io.IOException;
import java.nio.file.Path;

public interface Extractor {

    void extract(
            Path file,
            DocumentType documentType,
            RecordSink<ExtractedText> out,
            RecordSink<ExtractionError> err) throws IOException;
}
