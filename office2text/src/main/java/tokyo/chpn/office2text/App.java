package tokyo.chpn.office2text;

import org.apache.poi.openxml4j.util.ZipSecureFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tokyo.chpn.office2text.core.*;
import tokyo.chpn.office2text.extract.XlsxExtractor;
import tokyo.chpn.office2text.io.JacksonJsonLineWriter;
import tokyo.chpn.office2text.io.ObjectMapperFactory;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Hello world!
 */
public class App {

    private static final Logger LOGGER = LoggerFactory.getLogger(App.class);

    private final FormatDetector detector;
    private final ExtractorRegistry registry;
    private final JacksonJsonLineWriter<ExtractedText> outWriter;
    private final JacksonJsonLineWriter<ExtractionError> errWriter;

    private App (
            FormatDetector detector,
            ExtractorRegistry registry,
            JacksonJsonLineWriter<ExtractedText> outWriter,
            JacksonJsonLineWriter<ExtractionError> errWriter) {
        this.detector = detector;
        this.registry = registry;
        this.outWriter = outWriter;
        this.errWriter = errWriter;
    }

    public static void main(String[] args) {
        int exitCode;
        try {
            var objectMapper = ObjectMapperFactory.getInstance();
            App app = new App(
                    new ExtensionBasedFormatDetector(),
                    new DefaultExtractorRegistry(new XlsxExtractor()),
                    new JacksonJsonLineWriter<>(objectMapper, new OutputStreamWriter(System.out, StandardCharsets.UTF_8)),
                    new JacksonJsonLineWriter<>(objectMapper, new OutputStreamWriter(System.err, StandardCharsets.UTF_8))
            );
            exitCode = app.run(args);
        } catch (IOException e) {
            LOGGER.error("I/O failure: {}", e.getMessage());
            exitCode = 1;
        } catch (Exception e) {
            LOGGER.error("Unexpected error: {}", e.getMessage());
            exitCode = 1;
        }
        System.exit(exitCode);
    }

    public int run (String[] args) throws IOException {
        if (args.length == 0) {
            LOGGER.error("Usage: java -jar office2text.jar <file>");
            return 2;
        }
        ZipSecureFile.setMinInflateRatio(0);

        var hasError = false;

        for (String sourceFile : args) {
            var result = processOneFile(sourceFile);
            hasError |= (result == ProcessResult.FAILURE);
        }

        return hasError ? 1 : 0;
    }

    private ProcessResult processOneFile (String sourceFile) throws IOException {

        var file = Path.of(sourceFile);

        var documentTypeOpt = detector.detect(file);
        if (documentTypeOpt.isEmpty()) {
            errWriter.write(ExtractionError.unsupportedFileType(sourceFile));
            return ProcessResult.FAILURE;
        }

        var extractorOpt = registry.get(documentTypeOpt.get());
        if (extractorOpt.isEmpty()) {
            errWriter.write(ExtractionError.unsupportedFileType(sourceFile));
            return ProcessResult.FAILURE;
        }

        AtomicReference<ProcessResult> result = new AtomicReference<>(ProcessResult.SUCCESS);

        extractorOpt.get().extract(file, documentTypeOpt.get(), outWriter::write, error -> {
            result.set(ProcessResult.FAILURE);
            errWriter.write(error);
        });

        return result.get();
    }

}
