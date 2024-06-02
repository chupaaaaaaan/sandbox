package tokyo.chpn.office2text.extract.content.plain;

import tokyo.chpn.office2text.extract.content.Greppable;

public record PlainTextLine(String line) implements Greppable {
    @Override
    public String getGrepTarget() {
        return line;
    }

    @Override
    public String getPrintable() {
        return "-\t-\t" + line
                .replaceAll("\t", " ")
                .replaceAll("\r\n", " ")
                .replaceAll("\r", " ")
                .replaceAll("\n", " ");
    }

    @Override
    public boolean isError() {
        return false;
    }
}
