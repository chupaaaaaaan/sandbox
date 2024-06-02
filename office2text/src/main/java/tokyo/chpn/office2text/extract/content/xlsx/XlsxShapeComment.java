package tokyo.chpn.office2text.extract.content.xlsx;

import tokyo.chpn.office2text.extract.content.Greppable;

public record XlsxShapeComment(String sheetName, String shapeType, String shapeComment, boolean isError) implements Greppable {
    @Override
    public String getGrepTarget() {
        return shapeComment;
    }

    @Override
    public String getPrintable() {
        return sheetName + "\t" + shapeType + "\t" + shapeComment
                .replaceAll("\t", " ")
                .replaceAll("\r\n", " ")
                .replaceAll("\r", " ")
                .replaceAll("\n", " ");
    }
}
