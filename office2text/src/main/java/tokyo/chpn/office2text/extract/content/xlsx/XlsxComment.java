package tokyo.chpn.office2text.extract.content.xlsx;

import org.apache.poi.ss.util.CellAddress;
import tokyo.chpn.office2text.extract.content.Greppable;

public record XlsxComment(String sheetName, CellAddress address, String comment, boolean isError) implements Greppable {
    @Override
    public String getGrepTarget() {
        return comment;
    }

    @Override
    public String getPrintable() {
        return sheetName + "\t" + address.toString() + "\t" + comment
                .replaceAll("\t", " ")
                .replaceAll("\r\n", " ")
                .replaceAll("\r", " ")
                .replaceAll("\n", " ");

    }
}
