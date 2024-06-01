package tokyo.chpn.office2text.unit.xlsx;

import org.apache.poi.ss.util.CellAddress;
import tokyo.chpn.office2text.unit.ProcessingUnit;

public record XlsxCellValue(String sheetName, CellAddress address, String cellValue) implements ProcessingUnit {
    @Override
    public String getUnit() {
        return cellValue;
    }
}
