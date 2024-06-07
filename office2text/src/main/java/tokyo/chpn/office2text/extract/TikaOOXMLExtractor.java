package tokyo.chpn.office2text.extract;

import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.microsoft.ooxml.OOXMLParser;
import org.apache.tika.sax.BodyContentHandler;
import org.xml.sax.SAXException;
import tokyo.chpn.office2text.extract.content.Greppable;
import tokyo.chpn.office2text.extract.content.plain.PlainTextLine;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public final class TikaOOXMLExtractor {

    private TikaOOXMLExtractor() {}

    public static List<Greppable> extract(Path targetFilePath) {

        List<Greppable> targetStrings = new ArrayList<>();

        try (InputStream is = Files.newInputStream(targetFilePath)) {
            BodyContentHandler handler = new BodyContentHandler(-1);
            Metadata metadata = new Metadata();
            ParseContext context = new ParseContext();

            OOXMLParser parser = new OOXMLParser();
            parser.parse(is, handler, metadata, context);

            try (Scanner scanner = new Scanner(handler.toString())){
                while (scanner.hasNextLine()) {
                    String line = scanner.nextLine();
                    if (line.trim().isEmpty()) continue;
                    targetStrings.add(new PlainTextLine(line.trim()));
                }
            }
        } catch (IOException | TikaException | SAXException e) {
            throw new RuntimeException(e);
        }

        return targetStrings;
    }
}