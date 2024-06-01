package tokyo.chpn.office2text.extract;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.*;

class XlsxExtractorTest {


    @BeforeEach
    void setUp() {
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    public void searchTextFromExcel() {
        ClassLoader classLoader = getClass().getClassLoader();
        String targetFileName = classLoader.getResource("data/ui-standard-parts-catalog.xlsx").getPath();
        Set<Predicate<String>> predicates = new HashSet<>();

        predicates.add(line -> line.contains("Semantic"));

        XlsxExtractor extractor = new XlsxExtractor(targetFileName, predicates);

        List<String> result = extractor.searchOnExcel();

        assertEquals(2, result.size());
    }



}