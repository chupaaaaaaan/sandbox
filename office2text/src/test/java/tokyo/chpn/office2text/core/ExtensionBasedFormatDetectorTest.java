package tokyo.chpn.office2text.core;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
class ExtensionBasedFormatDetectorTest {

    @ParameterizedTest
    @MethodSource("dataProvider")
    void detect_returnsExpectedResult(Path path, Optional<DocumentType> expected) {
        Optional<DocumentType> actual = new ExtensionBasedFormatDetector().detect(path);
        assertEquals(expected, actual);
    }

    static Stream<Arguments> dataProvider() {
        return Stream.of(
                arguments(Paths.get("sample.xlsx"), Optional.of(DocumentType.XLSX)),
                arguments(Paths.get("sample.xlsm"), Optional.of(DocumentType.XLSM)),
                arguments(Paths.get("sample.txt"), Optional.empty())
        );
    }
}