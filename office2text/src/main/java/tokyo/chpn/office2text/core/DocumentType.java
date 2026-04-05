package tokyo.chpn.office2text.core;

public enum DocumentType {

    XLSX("xlsx"),
    XLSM("xlsm"),
    DOCX("docx");

    private final String jsonValue;

    DocumentType(String jsonValue) {
        this.jsonValue = jsonValue;
    }

    public String jsonValue() {
        return jsonValue;
    }
}
