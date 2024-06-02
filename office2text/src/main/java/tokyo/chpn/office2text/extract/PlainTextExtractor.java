package tokyo.chpn.office2text.extract;

import tokyo.chpn.office2text.extract.content.Greppable;
import tokyo.chpn.office2text.extract.content.plain.PlainTextLine;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class PlainTextExtractor {

    private PlainTextExtractor() {
    }

    public static List<Greppable> extract(Path targetFilePath) {

        List<Greppable> targetStrings = new ArrayList<>();

        try (Stream<String> lineStream = Files.lines(targetFilePath)) {

            lineStream.forEach(l -> {
                if (!l.trim().isEmpty()) {
                    targetStrings.add(new PlainTextLine(l.trim()));
                }
            });

        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return targetStrings;
    }

}
