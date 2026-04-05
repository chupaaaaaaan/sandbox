package tokyo.chpn.office2text.core;

import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;

public final class DefaultExtractorRegistry implements ExtractorRegistry {
    private final Map<DocumentType, Extractor> extractors;

    public DefaultExtractorRegistry(Extractor xlsxExtractor) {
        EnumMap<DocumentType, Extractor> map = new EnumMap<>(DocumentType.class);
        map.put(DocumentType.XLSX, xlsxExtractor);
        map.put(DocumentType.XLSM, xlsxExtractor);
        this.extractors = Map.copyOf(map);
    }

    @Override
    public Optional<Extractor> get(DocumentType documentType) {
        return Optional.ofNullable(extractors.get(documentType));
    }
}