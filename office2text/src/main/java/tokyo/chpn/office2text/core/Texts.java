package tokyo.chpn.office2text.core;

public final class Texts {
    private Texts() {}

    public static boolean shouldEmit(String text) {
        return text != null && !text.isBlank();
    }
}
