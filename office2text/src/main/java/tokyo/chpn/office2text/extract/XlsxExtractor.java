package tokyo.chpn.office2text.extract;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import tokyo.chpn.office2text.core.*;

import java.io.IOException;
import java.nio.file.Path;

public final class XlsxExtractor implements Extractor {

    @Override
    public void extract(Path file, DocumentType documentType, RecordSink<ExtractedText> out, RecordSink<ExtractionError> err) throws IOException {
        String sourceFile = file.toString();

        try (OPCPackage pkg = OPCPackage.open(file.toFile());
             Workbook workbook = new XSSFWorkbook(pkg)) {

            DataFormatter formatter = new DataFormatter();
            FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();

            for (Sheet sheet : workbook) {
                extractSheet(sourceFile, documentType, sheet, out, err, formatter, evaluator);
            }
        } catch (InvalidFormatException e) {
            ExtractionError extractionError = ExtractionError.of(sourceFile,
                    documentType,
                    Stages.LOAD_DOCUMENT,
                    null,
                    null,
                    "Failed to load file.",
                    e,
                    null);
            err.accept(extractionError);
        }
    }


    private void extractSheet(String sourceFile, DocumentType documentType, Sheet sheet, RecordSink<ExtractedText> out, RecordSink<ExtractionError> err, DataFormatter formatter, FormulaEvaluator evaluator) throws IOException {
        extractCells(sourceFile, documentType,sheet, out, err, formatter, evaluator);
    }

    private void extractCells(String sourceFile, DocumentType documentType, Sheet sheet, RecordSink<ExtractedText> out, RecordSink<ExtractionError> err, DataFormatter formatter, FormulaEvaluator evaluator) throws IOException {

        for (Row row : sheet) {
            for (Cell cell : row) {
                ExtractedText extractedText;
                try {
                    String cellValue = extractCell(cell, formatter, evaluator);
                    if (cellValue.trim().isEmpty()) continue;
                    extractedText = ExtractedText.of(sourceFile,
                            documentType,
                            PartTypes.CELL,
                            sheet.getSheetName(),
                            cell.getAddress().toString(),
                            cellValue,
                            null);
                    out.accept(extractedText);
                } catch (RuntimeException e) {
                    ExtractionError extractionError = ExtractionError.of(sourceFile,
                            documentType,
                            Stages.EXTRACT_CELL,
                            sheet.getSheetName(),
                            cell.getAddress().toString(),
                            "Failed to extract cell.",
                            e,
                            null);
                    err.accept(extractionError);
                }
            }
        }
    }


    // private XlsxExtractor() {
    // }

    // public static List<Greppable> extract(Path targetFilePath) {

    //     List<Greppable> targetStrings = new ArrayList<>();

    //     try (OPCPackage pkg = OPCPackage.open(targetFilePath.toFile());
    //          Workbook workbook = new XSSFWorkbook(pkg)) {

    //         DataFormatter formatter = new DataFormatter();
    //         FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();

    //         for (Sheet sheet : workbook) {
    //             for (Row row : sheet)
    //                 for (Cell cell : row) {
    //                     // Cell value
    //                     try {
    //                         String cellValue = extractCell(cell, formatter, evaluator);
    //                         if (cellValue.trim().isEmpty()) continue;
    //                         targetStrings.add(new XlsxCellValue(sheet.getSheetName(), cell.getAddress(), cellValue, false));
    //                     } catch (Exception e) {
    //                         targetStrings.add(new XlsxCellValue(sheet.getSheetName(), cell.getAddress(), e.getMessage(), true));
    //                     }

    //                 }

    //             if (!(sheet instanceof XSSFSheet xssfSheet)) continue;

    //             // Comments
    //             Map<CellAddress, XSSFComment> commentsTable = xssfSheet.getCellComments();
    //             for (Map.Entry<CellAddress, XSSFComment> entry : commentsTable.entrySet()) {
    //                 CellAddress cellAddress = entry.getKey();
    //                 XSSFComment comment = entry.getValue();

    //                 targetStrings.add(new XlsxComment(sheet.getSheetName(), cellAddress, comment.getString().getString(), false));
    //             }

    //             // Objects
    //             XSSFDrawing drawing = xssfSheet.getDrawingPatriarch();
    //             if (drawing == null) continue;

    //             for (XSSFShape shape : drawing.getShapes()) {
    //                 processShape(shape, targetStrings, sheet.getSheetName());
    //             }
    //         }
    //     } catch (IOException | InvalidFormatException e) {
    //         throw new RuntimeException(e);

    //     }

    //     return targetStrings;
    // }

    // private static void processShape(XSSFShape shape, List<Greppable> targetStrings, String sheetName) {
    //     if (shape instanceof XSSFTextBox textBox) {
    //         String text = textBox.getText();
    //         if (text != null && !text.isEmpty()) {
    //             targetStrings.add(new XlsxShapeComment(sheetName, "TextBox", text, false));
    //         }
    //     } else if (shape instanceof XSSFSimpleShape simpleShape) {
    //         String shapeString = simpleShape.getText();
    //         if (shapeString != null && !shapeString.isEmpty()) {
    //             targetStrings.add(new XlsxShapeComment(sheetName, "SimpleShape", shapeString, false));
    //         }
    //     } else if (shape instanceof XSSFShapeGroup shapeGroup) {
    //         for (XSSFShape childShape : shapeGroup) {
    //             processShape(childShape, targetStrings, sheetName);
    //         }
    //     }
    // }

     private static String extractCell(Cell cell, DataFormatter formatter, FormulaEvaluator evaluator) {
         return switch (cell.getCellType()) {
             case BLANK -> "";
             case BOOLEAN, NUMERIC -> formatter.formatCellValue(cell);
             case FORMULA -> formatter.formatCellValue(cell, evaluator);
             case STRING -> cell.getStringCellValue();
             case ERROR -> throw new IllegalStateException("Error:" + FormulaError.forInt(cell.getErrorCellValue()));
             case _NONE -> "Unknown type";
         };
     }
}