package tokyo.chpn.office2text.core;

public final class Stages {
    private Stages() {
    }

    public static final String DETECT_FORMAT = "detect-format";
    public static final String OPEN_PACKAGE = "open-package";

    public static final String LOAD_WORKBOOK = "load-workbook";
    public static final String LOAD_SHEET = "load-sheet";
    public static final String EXTRACT_CELL = "extract-cell";
    public static final String EXTRACT_COMMENT = "extract-comment";
    public static final String EXTRACT_SHAPE = "extract-shape";

    public static final String LOAD_DOCUMENT = "load-document";
    public static final String EXTRACT_PARAGRAPH = "extract-paragraph";
    public static final String EXTRACT_TABLE_CELL = "extract-table-cell";
}
