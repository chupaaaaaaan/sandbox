package tokyo.chpn.office2text.filter;

import tokyo.chpn.office2text.extract.content.Greppable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class TextFilter {

    private TextFilter() {}

    public static Predicate<Greppable> filter(Condition cond) {
        return greppable -> !greppable.isError() && cond.predicate().test(greppable.getGrepTarget());
    }

    public static Predicate<Greppable> filterError() {
        return Greppable::isError;
    }
}
