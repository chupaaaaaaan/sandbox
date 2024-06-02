package tokyo.chpn.office2text;

import org.apache.poi.openxml4j.util.ZipSecureFile;
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
import java.util.stream.Stream;

/**
 * Hello world!
 */
public class App {

    public static void main(String[] args) {

        ZipSecureFile.setMinInflateRatio(0);

        Path targetFilePath = Paths.get(args[0]);
        List<Condition> conditions = new ArrayList<>() {{
            add(Condition.createCaseInsensitiveWordMatchCondition("JSP", "jsp", "\\w\\.jsp"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JSF", "jsf"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JASPIC", "jaspic"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JACC", "jacc"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JMS", "jms"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JPA", "jpa"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JTA", "jta"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JBatch", "jbatch"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JCA", "jca"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JAF", "jaf"));
            add(Condition.createCaseInsensitiveWordMatchCondition("EL", "el", "\\w\\.el", "el\\s*\\)"));
            add(Condition.createCaseInsensitiveWordMatchCondition("EBJ", "ebj"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JAXB", "jaxb"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JSON-B", "json-b"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JSON-P", "json-p"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JAX-WS", "jax-ws"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JAX-WS", "jax-ws"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JAX-RS", "jax-rs"));
            add(Condition.createCaseInsensitiveWordMatchCondition("JSTL", "jstl"));
            add(Condition.createCaseInsensitiveWordMatchCondition("CDI", "cdi"));
        }};

        List<Greppable> extracted = XlsxExtractor.extract(targetFilePath);

        for (Condition condition : conditions) {

            List<Filtered> result = extracted.stream().filter(TextFilter.filter(condition)).map(g -> new Filtered(condition, g)).toList();
            List<Greppable> errorResult = extracted.stream().filter(TextFilter.filterError()).toList();

            try (BufferedWriter fileNameList = Files.newBufferedWriter(Paths.get("./fileNameList.txt"), StandardOpenOption.APPEND, StandardOpenOption.CREATE);
                 BufferedWriter detailsList = Files.newBufferedWriter(Paths.get("./detailsList.txt"), StandardOpenOption.APPEND, StandardOpenOption.CREATE);
                 BufferedWriter errorList = Files.newBufferedWriter(Paths.get("./errorList.txt"), StandardOpenOption.APPEND, StandardOpenOption.CREATE)) {

                if (!result.isEmpty()) {
                    fileNameList.write(condition.description() + "\t" + targetFilePath + "\n");
                }

                for (Filtered unit : result) {
                    detailsList.write(targetFilePath + "\t" + unit.condition().description() + "\t" + unit.greppable().getPrintable() + "\n");
                }

                for (Greppable g : errorResult) {
                    errorList.write(targetFilePath + "\t" + g.getPrintable() + "\n");
                }

            } catch (IOException e) {
                throw new RuntimeException(e);
            }

        }
    }
}
