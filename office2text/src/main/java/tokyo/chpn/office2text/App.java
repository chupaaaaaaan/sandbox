package tokyo.chpn.office2text;

import org.apache.commons.io.FilenameUtils;
import org.apache.poi.openxml4j.util.ZipSecureFile;
import tokyo.chpn.office2text.extract.DocxExtractor;
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

    public static void main(String[] args) {

        ZipSecureFile.setMinInflateRatio(0);

        Path targetFilePath = Paths.get(args[0]);
        List<Condition> conditions = new ArrayList<>() {{
            add(Condition.createCaseInsensitiveWordMatch("JSP", "jsp", "\\w\\.jsp"));
            add(Condition.createCaseInsensitiveWordMatch("JSF", "jsf"));
            add(Condition.createCaseInsensitiveWordMatch("JASPIC", "jaspic"));
            add(Condition.createCaseInsensitiveWordMatch("JACC", "jacc"));
            add(Condition.createCaseInsensitiveWordMatch("JMS", "jms"));
            add(Condition.createCaseInsensitiveWordMatch("JPA", "jpa"));
            add(Condition.createCaseInsensitiveWordMatch("JTA", "jta"));
            add(Condition.createCaseInsensitiveWordMatch("JBatch", "jbatch"));
            add(Condition.createCaseInsensitiveWordMatch("JCA", "jca"));
            add(Condition.createCaseInsensitiveWordMatch("JAF", "jaf"));
            add(Condition.createCaseInsensitiveWordMatch("EL", "el", "\\w\\.el", "el\\s*\\)"));
            add(Condition.createCaseInsensitiveWordMatch("EBJ", "ebj"));
            add(Condition.createCaseInsensitiveWordMatch("JAXB", "jaxb"));
            add(Condition.createCaseInsensitiveWordMatch("JSON-B", "json-b"));
            add(Condition.createCaseInsensitiveWordMatch("JSON-P", "json-p"));
            add(Condition.createCaseInsensitiveWordMatch("JAX-WS", "jax-ws"));
            add(Condition.createCaseInsensitiveWordMatch("JAX-WS", "jax-ws"));
            add(Condition.createCaseInsensitiveWordMatch("JAX-RS", "jax-rs"));
            add(Condition.createCaseInsensitiveWordMatch("JSTL", "jstl"));
            add(Condition.createCaseInsensitiveWordMatch("CDI", "cdi"));
        }};

        List<Greppable> extracted = switch (FilenameUtils.getExtension(targetFilePath.toString())) {
            case "xlsx", "xlsm" -> XlsxExtractor.extract(targetFilePath);
            case "docx" -> DocxExtractor.extract(targetFilePath);
            default -> PlainTextExtractor.extract(targetFilePath);
        };

        process(conditions, extracted, targetFilePath);
    }


    public static void process(List<Condition> conditions, List<Greppable> extracted, Path targetFilePath) {

        try (BufferedWriter fileNameList = Files.newBufferedWriter(Paths.get("./fileNameList.txt"), StandardOpenOption.APPEND, StandardOpenOption.CREATE);
             BufferedWriter detailsList = Files.newBufferedWriter(Paths.get("./detailsList.txt"), StandardOpenOption.APPEND, StandardOpenOption.CREATE);
             BufferedWriter errorList = Files.newBufferedWriter(Paths.get("./errorList.txt"), StandardOpenOption.APPEND, StandardOpenOption.CREATE)) {

            for (Condition condition : conditions) {

                List<Filtered> result = extracted.stream().filter(TextFilter.filter(condition)).map(g -> new Filtered(condition, g)).toList();
                List<Greppable> errorResult = extracted.stream().filter(TextFilter.filterError()).toList();

                if (!result.isEmpty()) {
                    fileNameList.write(condition.description() + "\t" + targetFilePath + "\n");
                }

                for (Filtered unit : result) {
                    detailsList.write(targetFilePath + "\t" + unit.condition().description() + "\t" + unit.greppable().getPrintable() + "\n");
                }

                for (Greppable g : errorResult) {
                    errorList.write(targetFilePath + "\t" + g.getPrintable() + "\n");
                }


            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
