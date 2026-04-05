package tokyo.chpn.office2text.core;

import java.nio.file.Path;
import java.util.Optional;

public interface FormatDetector {
    Optional<DocumentType> detect(Path file);
}
    
