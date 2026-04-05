package tokyo.chpn.office2text;

import org.apache.poi.openxml4j.util.ZipSecureFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tokyo.chpn.office2text.core.DefaultExtractorRegistry;
import tokyo.chpn.office2text.core.ExtensionBasedFormatDetector;
import tokyo.chpn.office2text.core.ExtractedText;
import tokyo.chpn.office2text.core.ExtractionError;
import tokyo.chpn.office2text.extract.XlsxExtractor;
import tokyo.chpn.office2text.io.JacksonJsonLineWriter;
import tools.jackson.core.StreamWriteFeature;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Hello world!
 */
public class App {

    private static final Logger LOGGER = LoggerFactory.getLogger(App.class);

    public static void main(String[] args) {
        int exitCode;
        try {
            exitCode = new App().run(args);
        } catch (IOException e) {
            e.printStackTrace();
            LOGGER.error("I/O failure: {}", e.getMessage());
            exitCode = 1;
        } catch (Exception e) {
            e.printStackTrace();
            LOGGER.error("Unexpected error: {}", e.getMessage());
            exitCode = 1;
        }
        System.exit(exitCode);
    }

    public int run (String[] args) throws IOException {
        if (args.length != 1) {
            LOGGER.error("Usage: java -jar office2text.jar <file>");
            System.exit(2);
        }
        ZipSecureFile.setMinInflateRatio(0);

        String sourceFile = args[0];
        var file = Path.of(sourceFile);

        var objectMapper = JsonMapper.builder()
                .disable(SerializationFeature.INDENT_OUTPUT)
                .disable(StreamWriteFeature.AUTO_CLOSE_TARGET)
                .build();

        var outWriter = new JacksonJsonLineWriter<ExtractedText>(objectMapper, new OutputStreamWriter(System.out));
        var errWriter = new JacksonJsonLineWriter<ExtractionError>(objectMapper, new OutputStreamWriter(System.err));

        var detector = new ExtensionBasedFormatDetector();
        var registry = new DefaultExtractorRegistry(new XlsxExtractor());

        var documentTypeOpt = detector.detect(file);
        if (documentTypeOpt.isEmpty()) {
            errWriter.write(ExtractionError.unsupportedFileType(sourceFile));
            return 1;
        }

        var extractorOpt = registry.get(documentTypeOpt.get());
        if (extractorOpt.isEmpty()) {
            errWriter.write(ExtractionError.unsupportedFileType(sourceFile));
            return 1;
        }

        var hasError = new AtomicBoolean(false);

        extractorOpt.get().extract(file, documentTypeOpt.get(), outWriter::write, error -> {
            hasError.set(true);
            errWriter.write(error);
        });

        return hasError.get() ? 0 : 1;
    }
}
