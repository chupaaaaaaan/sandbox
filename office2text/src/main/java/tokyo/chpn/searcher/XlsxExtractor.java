package tokyo.chpn.searcher;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class XlsxExtractor {

    private final String targetFilePath;
    private final Set<Predicate<String>> conditions;

    public XlsxExtractor(String targetFilePath, String conditionFilePath) {
        this.targetFilePath = targetFilePath;
        this.conditions = constructConditions(conditionFilePath);
    }

    public XlsxExtractor(String targetFilePath, Set<Predicate<String>> conditions) {
        this.targetFilePath = targetFilePath;
        this.conditions = conditions;
    }

    public List<String> searchOnExcel() {
        return search(extract());
    }

    private Set<Predicate<String>> constructConditions(String conditionFile) {

        try (Stream<String> lines = Files.lines(Path.of(conditionFile))) {
            return lines.map(l -> (Predicate<String>) (String t) -> t.toLowerCase().contains(l.toLowerCase())).collect(Collectors.toUnmodifiableSet());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private List<String> search(List<String> targetStrings) {

        List<String> result = new ArrayList<>();

        for(String targetString : targetStrings) {
            System.out.println(targetString);
            for(Predicate<String> condition : conditions) {
                if(condition.test(targetString)) {
                    result.add(targetString);
                }
            }
        }
        return result;
    }



    private List<String> extract() {

        List<String> targetStrings = new ArrayList<>();

        try (OPCPackage pkg = OPCPackage.open(targetFilePath);
             Workbook workbook = new XSSFWorkbook(pkg)) {

            DataFormatter formatter = new DataFormatter();
            FormulaEvaluator evaluator = workbook.getCreationHelper().createFormulaEvaluator();

            for (Sheet sheet : workbook) {
                for (Row row : sheet) for (Cell cell : row) {
                    // Cell value
                    String cellValue
                            = cell.getCellType() == CellType.FORMULA
                            ? formatter.formatCellValue(cell, evaluator)
                            : formatter.formatCellValue(cell);
                    if (cellValue.trim().isEmpty()) continue;
                    targetStrings.add(cellValue);

                    // Comments
                    Comment cellComment = cell.getCellComment();
                    if (cellComment == null) continue;

                    String commentString = cellComment.getString().getString();
                    if (commentString.isEmpty()) continue;

                    targetStrings.add(commentString);
                }

                // Objects
                if (!(sheet instanceof XSSFSheet xssfSheet)) continue;;
                XSSFDrawing drawing = xssfSheet.getDrawingPatriarch();
                if (drawing == null) continue;
                for (XSSFShape shape : drawing.getShapes()) {
                    if (!(shape instanceof XSSFSimpleShape simpleShape)) continue;;

                    String shapeString = simpleShape.getText();
                    if (shapeString == null || shapeString.isEmpty()) continue;

                    targetStrings.add(shapeString);
                }
            }
        } catch (IOException | InvalidFormatException e) {
            throw new RuntimeException(e);

        }

        return targetStrings;
    }
}