package tokyo.chpn.office2text;

import org.apache.commons.io.FilenameUtils;
import org.apache.poi.openxml4j.util.ZipSecureFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tokyo.chpn.office2text.extract.TikaOOXMLExtractor;
import tokyo.chpn.office2text.extract.PlainTextExtractor;
import tokyo.chpn.office2text.extract.XlsxExtractor;
import tokyo.chpn.office2text.extract.content.Greppable;
import tokyo.chpn.office2text.filter.Condition;
import tokyo.chpn.office2text.filter.TextFilter;
import tokyo.chpn.office2text.filter.Filtered;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

/**
 * Hello world!
 */
public class App {

    private static final Logger LOGGER = LoggerFactory.getLogger(App.class);

    public static void main(String[] args) {

        if (args.length != 1) {
            LOGGER.error("Usage: java -jar office2text.jar <file>");
            System.exit(1);
        }

        ZipSecureFile.setMinInflateRatio(0);

        List<Condition> conditions = new ArrayList<>() {{
            add(Condition.createCaseInsensitiveWordMatch("JSP", "jsp",
                    "\\.jsp", "jsp\\.\\w", "jsp:include", "<jsp:", "`jsp", "<jsp", "</jsp", "http://java\\.sun\\.com/jsp"));
            add(Condition.createCaseInsensitiveWordMatch("JSF", "jsf"));
            add(Condition.createCaseInsensitiveWordMatch("JASPIC", "jaspic"));
            add(Condition.createCaseInsensitiveWordMatch("JACC", "jacc"));
            add(Condition.createCaseInsensitiveWordMatch("JMS", "jms",
                    "javax\\.jms\\."));
            add(Condition.createCaseInsensitiveWordMatch("JPA", "jpa"));
            add(Condition.createCaseInsensitiveWordMatch("JTA", "jta"));
            add(Condition.createCaseInsensitiveWordMatch("JBatch", "jbatch"));
            add(Condition.createCaseInsensitiveWordMatch("JCA", "jca"));
            add(Condition.createCaseInsensitiveWordMatch("JAF", "jaf"));
            add(Condition.createCaseInsensitiveWordMatch("EL", "el",
                    "\\w\\.el", "el\\s*\\)"));
            add(Condition.createCaseInsensitiveWordMatch("EBJ", "ebj"));
            add(Condition.createCaseInsensitiveWordMatch("JAXB", "jaxb"));
            add(Condition.createCaseInsensitiveWordMatch("JSON-B", "json-b"));
            add(Condition.createCaseInsensitiveWordMatch("JSON-P", "json-p"));
            add(Condition.createCaseInsensitiveWordMatch("JAX-WS", "jax-ws"));
            add(Condition.createCaseInsensitiveWordMatch("JAX-RS", "jax-rs"));
            add(Condition.createCaseInsensitiveWordMatch("JSTL", "jstl",
                    "http://java\\.sun\\.com/jsp/jstl"));
            add(Condition.createCaseInsensitiveWordMatch("CDI", "cdi"));
            add(Condition.createCaseInsensitiveWordMatch("JSR352", "jsr352"));
            add(Condition.createCaseInsensitiveWordMatch("java.sun.com", "java\\.sun\\.com"));
        }};

        try (BufferedWriter fileNameList = Files.newBufferedWriter(Paths.get("./fileNameList.txt"), StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
             BufferedWriter detailsList = Files.newBufferedWriter(Paths.get("./detailsList.txt"), StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
             BufferedWriter errorList = Files.newBufferedWriter(Paths.get("./errorList.txt"), StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {

            List<String> targetFileNames = Files.readAllLines(Paths.get(args[0]));

            for (String targetFilePath : targetFileNames) {

                LOGGER.info("target file in process: {}", targetFilePath);
                Path path = Paths.get(targetFilePath);
                List<Greppable> extracted =
                        switch (FilenameUtils.getExtension(targetFilePath)) {
                            case "xlsx", "xlsm" -> XlsxExtractor.extract(path);
                            case "docx" -> TikaOOXMLExtractor.extract(path);
                            default -> PlainTextExtractor.extract(path);
                        };

                for (Condition condition : conditions) {

                    List<Filtered> result = extracted.stream().filter(TextFilter.filter(condition)).map(g -> new Filtered(condition, g)).toList();
                    List<Greppable> errorResult = extracted.stream().filter(TextFilter.filterError()).toList();

                    if (!result.isEmpty()) {
                        fileNameList.write(condition.description() + "\t" + path + "\n");
                    }

                    for (Filtered unit : result) {
                        detailsList.write(targetFilePath + "\t" + unit.condition().description() + "\t" + unit.greppable().getPrintable() + "\n");
                    }

                    for (Greppable g : errorResult) {
                        errorList.write(targetFilePath + "\t" + g.getPrintable() + "\n");
                    }
                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
