package tokyo.chpn.office2text.unit.xlsx;

import org.apache.poi.ss.util.CellAddress;
import tokyo.chpn.office2text.unit.ProcessingUnit;

public record XlsxComment(String sheetName, CellAddress cellAddress, String comment) implements ProcessingUnit {
    @Override
    public String getUnit() {
        return comment;
    }
}
