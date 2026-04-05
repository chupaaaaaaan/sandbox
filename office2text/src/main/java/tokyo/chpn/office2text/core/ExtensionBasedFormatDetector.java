package tokyo.chpn.office2text.core;

import java.nio.file.Path;
import java.util.Locale;
import java.util.Optional;

public final class ExtensionBasedFormatDetector implements FormatDetector {

    @Override
    public Optional<DocumentType> detect(Path file) {
        String name = file.getFileName().toString().toLowerCase(Locale.ROOT);

        if (name.endsWith(".xlsx")) {
            return Optional.of(DocumentType.XLSX);
        }

        if (name.endsWith(".xlsm")) {
            return Optional.of(DocumentType.XLSM);
        }

        if (name.endsWith(".docx")) {
            return Optional.of(DocumentType.DOCX);
        }

        return Optional.empty();
    }
}
