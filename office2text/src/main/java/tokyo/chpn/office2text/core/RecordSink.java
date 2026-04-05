package tokyo.chpn.office2text.core;

import java.io.IOException;

@FunctionalInterface
public interface RecordSink<T> {
    void accept(T value) throws IOException;
}
