package tokyo.chpn.office2text.core;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.*;

class TextsTest {

    @ParameterizedTest
    @CsvSource(value = {
            "NULL,     false",
            "'',       false",
            "'   ',    false",
            "'\t',     false",
            "'\n',     false",
            "'\r\n',   false",
            "' \t \n ',false",
            "'a',      true",
            "' a ',    true",
            "'0',      true",
            "'false',  true",
            "'　',     false"
    }, nullValues = "NULL")
    void shouldEmit_returnExpectedResult(String text, boolean expected) {
        boolean actual = Texts.shouldEmit(text);
        assertEquals(expected, actual);
    }
}