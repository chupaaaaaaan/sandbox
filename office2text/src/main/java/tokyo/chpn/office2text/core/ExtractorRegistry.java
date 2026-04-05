package tokyo.chpn.office2text.core;

import java.util.Optional;

public interface ExtractorRegistry {
    Optional<Extractor> get(DocumentType documentType);
}
