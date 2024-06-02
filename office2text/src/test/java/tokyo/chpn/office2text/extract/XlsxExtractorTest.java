package tokyo.chpn.office2text.extract;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import tokyo.chpn.office2text.filter.Condition;
import tokyo.chpn.office2text.filter.TextFilter;
import tokyo.chpn.office2text.filter.Filtered;

import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;

class XlsxExtractorTest {


    @BeforeEach
    void setUp() {
    }

    @AfterEach
    void tearDown() {
    }


    private int countMatches(Pattern pattern, String text) {
        Matcher matcher = pattern.matcher(text);
        int count = 0;
        while (matcher.find()) {
            count++;
        }
        return count;
    }


    @Test
    public void searchTextFromExcel() throws URISyntaxException {
        ClassLoader classLoader = getClass().getClassLoader();
        URL targetFileURL = classLoader.getResource("data/ui-standard-parts-catalog.xlsx");
        assertNotNull(targetFileURL);
        Path path = Paths.get(targetFileURL.toURI());

        Condition condition = Condition.createCaseInsensitiveWordMatchCondition("Nablarch", "Nablarch", "nablarch\\.github\\.io", "nablarch-example-web");
//        Condition condition = Condition.createPhraseMatchCondition("Nablarch未提供", "Nablarch未提供");

        List<Filtered> result =  XlsxExtractor.extract(path).stream().filter(TextFilter.filter(condition)).map(g -> new Filtered(condition, g)).toList();
        assertEquals(53, result.size());
    }



}