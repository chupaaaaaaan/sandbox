package tokyo.chpn.office2text.extract.content.xlsx;

import org.apache.poi.ss.util.CellAddress;
import tokyo.chpn.office2text.extract.content.Greppable;

public record XlsxCellValue(String sheetName, CellAddress address, String cellValue,
                            boolean isError) implements Greppable {
    @Override
    public String getGrepTarget() {
        return cellValue;
    }

    @Override
    public String getPrintable() {
        return sheetName + "\t" + address.toString() + "\t" + cellValue
                .replaceAll("\t", " ")
                .replaceAll("\r\n", " ")
                .replaceAll("\r", " ")
                .replaceAll("\n", " ");

    }


}
