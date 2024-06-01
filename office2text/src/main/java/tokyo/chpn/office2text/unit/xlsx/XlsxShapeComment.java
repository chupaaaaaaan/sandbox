package tokyo.chpn.office2text.unit.xlsx;

import tokyo.chpn.office2text.unit.ProcessingUnit;

public record XlsxShapeComment(String sheetName, String shapeComment) implements ProcessingUnit {
    @Override
    public String getUnit() {
        return shapeComment;
    }
}
