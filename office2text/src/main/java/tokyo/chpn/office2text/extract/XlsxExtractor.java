package tokyo.chpn.office2text.extract;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;
import tokyo.chpn.office2text.unit.ProcessingUnit;
import tokyo.chpn.office2text.unit.xlsx.XlsxCellValue;
import tokyo.chpn.office2text.unit.xlsx.XlsxComment;
import tokyo.chpn.office2text.unit.xlsx.XlsxShapeComment;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class XlsxExtractor {

    public List<ProcessingUnit> extract(String targetFilePath) {

        List<ProcessingUnit> targetStrings = new ArrayList<>();

        try (OPCPackage pkg = OPCPackage.open(targetFilePath);
             Workbook workbook = new XSSFWorkbook(pkg)) {

            DataFormatter formatter = new DataFormatter();
            FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();

            for (Sheet sheet : workbook) {
                for (Row row : sheet) for (Cell cell : row) {
                    // Cell value
                    String cellValue = switch (cell.getCellType()) {
                        case FORMULA -> formatter.formatCellValue(cell, evaluator);
                        default -> formatter.formatCellValue(cell);
                    };
                    if (cellValue.trim().isEmpty()) continue;
                    targetStrings.add(new XlsxCellValue(sheet.getSheetName(), cell.getAddress(), cellValue));

                    // Comments
                    Comment cellComment = cell.getCellComment();
                    if (cellComment == null) continue;

                    String commentString = cellComment.getString().getString();
                    if (commentString.isEmpty()) continue;

                    targetStrings.add(new XlsxComment(sheet.getSheetName(), cell.getAddress(), commentString));
                }

                // Objects
                if (!(sheet instanceof XSSFSheet xssfSheet)) continue;;
                XSSFDrawing drawing = xssfSheet.getDrawingPatriarch();
                if (drawing == null) continue;
                for (XSSFShape shape : drawing.getShapes()) {
                    if (!(shape instanceof XSSFSimpleShape simpleShape)) continue;;

                    String shapeString = simpleShape.getText();
                    if (shapeString == null || shapeString.isEmpty()) continue;

                    targetStrings.add(new XlsxShapeComment(sheet.getSheetName(), shapeString));
                }
            }
        } catch (IOException | InvalidFormatException e) {
            throw new RuntimeException(e);

        }

        return targetStrings;
    }
}