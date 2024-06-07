package tokyo.chpn.office2text.extract;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.FormulaError;
import org.apache.poi.ss.usermodel.FormulaEvaluator;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.xssf.usermodel.XSSFComment;
import org.apache.poi.xssf.usermodel.XSSFDrawing;
import org.apache.poi.xssf.usermodel.XSSFShape;
import org.apache.poi.xssf.usermodel.XSSFShapeGroup;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFSimpleShape;
import org.apache.poi.xssf.usermodel.XSSFTextBox;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import tokyo.chpn.office2text.extract.content.Greppable;
import tokyo.chpn.office2text.extract.content.xlsx.XlsxCellValue;
import tokyo.chpn.office2text.extract.content.xlsx.XlsxComment;
import tokyo.chpn.office2text.extract.content.xlsx.XlsxShapeComment;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public final class XlsxExtractor {

    private XlsxExtractor() {
    }

    public static List<Greppable> extract(Path targetFilePath) {

        List<Greppable> targetStrings = new ArrayList<>();

        try (OPCPackage pkg = OPCPackage.open(targetFilePath.toFile());
             Workbook workbook = new XSSFWorkbook(pkg)) {

            DataFormatter formatter = new DataFormatter();
            FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();

            for (Sheet sheet : workbook) {
                for (Row row : sheet)
                    for (Cell cell : row) {
                        // Cell value
                        try {
                            String cellValue = extractCell(cell, formatter, evaluator);
                            if (cellValue.trim().isEmpty()) continue;
                            targetStrings.add(new XlsxCellValue(sheet.getSheetName(), cell.getAddress(), cellValue, false));
                        } catch (Exception e) {
                            targetStrings.add(new XlsxCellValue(sheet.getSheetName(), cell.getAddress(), e.getMessage(), true));
                        }

                    }

                if (!(sheet instanceof XSSFSheet xssfSheet)) continue;

                // Comments
                Map<CellAddress, XSSFComment> commentsTable = xssfSheet.getCellComments();
                for (Map.Entry<CellAddress, XSSFComment> entry : commentsTable.entrySet()) {
                    CellAddress cellAddress = entry.getKey();
                    XSSFComment comment = entry.getValue();

                    targetStrings.add(new XlsxComment(sheet.getSheetName(), cellAddress, comment.getString().getString(), false));
                }

                // Objects
                XSSFDrawing drawing = xssfSheet.getDrawingPatriarch();
                if (drawing == null) continue;

                for (XSSFShape shape : drawing.getShapes()) {
                    processShape(shape, targetStrings, sheet.getSheetName());
                }
            }
        } catch (IOException | InvalidFormatException e) {
            throw new RuntimeException(e);

        }

        return targetStrings;
    }

    private static void processShape(XSSFShape shape, List<Greppable> targetStrings, String sheetName) {
        if (shape instanceof XSSFTextBox textBox) {
            String text = textBox.getText();
            if (text != null && !text.isEmpty()) {
                targetStrings.add(new XlsxShapeComment(sheetName, "TextBox", text, false));
            }
        } else if (shape instanceof XSSFSimpleShape simpleShape) {
            String shapeString = simpleShape.getText();
            if (shapeString != null && !shapeString.isEmpty()) {
                targetStrings.add(new XlsxShapeComment(sheetName, "SimpleShape", shapeString, false));
            }
        } else if (shape instanceof XSSFShapeGroup shapeGroup) {
            for (XSSFShape childShape : shapeGroup) {
                processShape(childShape, targetStrings, sheetName);
            }
        }
    }

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