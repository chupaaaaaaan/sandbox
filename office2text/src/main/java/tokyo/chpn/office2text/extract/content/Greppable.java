package tokyo.chpn.office2text.extract.content;

public interface Greppable {
    String getGrepTarget();

    String getPrintable();

    boolean isError();
}
