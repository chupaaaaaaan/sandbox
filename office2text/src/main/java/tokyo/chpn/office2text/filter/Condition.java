package tokyo.chpn.office2text.filter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.StringJoiner;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public record Condition(String description, Predicate<String> predicate) {

    private static final Logger LOGGER = LoggerFactory.getLogger(Condition.class);

    public static Condition createCaseInsensitiveWordMatch(String description, String word, String... otherExcludePattern) {
        Pattern pattern1 = Pattern.compile(word, Pattern.CASE_INSENSITIVE);
        Pattern pattern2 = Pattern.compile(makeExcludePattern(word, otherExcludePattern), Pattern.CASE_INSENSITIVE);

        LOGGER.debug("caseInsensitiveWordMatch description: {}, include: \"{}\", exclude: \"{}\"", description, pattern1.pattern(), pattern2.pattern());
        return new Condition(description, l -> countMatches(pattern1, l) != countMatches(pattern2, l));
    }

    public static Condition createWordMatch(String description, String word, String... otherExcludePatterns) {
        Pattern pattern1 = Pattern.compile(word);
        Pattern pattern2 = Pattern.compile(makeExcludePattern(word, otherExcludePatterns));

        LOGGER.debug("wordMatch description: {}, include: \"{}\", exclude: \"{}\"", description, pattern1.pattern(), pattern2.pattern());
        return new Condition(description, l -> countMatches(pattern1, l) != countMatches(pattern2, l));
    }

    private static String makeExcludePattern(String word, String... otherExcludePatterns) {
        StringJoiner excludePattern = new StringJoiner("|");
        excludePattern.add("\\w" + word);
        excludePattern.add(word + "\\w");
        for (String pattern : otherExcludePatterns) {
            if(!pattern.contains(word)) throw new IllegalArgumentException("otherExcludePattern must contain " + word);
            excludePattern.add(pattern);
        }
        return excludePattern.toString();
    }

    public static Condition createSimpleMatch(String matchString) {
        Pattern pattern = Pattern.compile(Pattern.quote(matchString));
        LOGGER.debug("simpleMatch pattern: \"{}\"", pattern.pattern());
        return new Condition(matchString, l -> pattern.matcher(l).matches());
    }

    private static int countMatches(Pattern pattern, String text) {
        Matcher matcher = pattern.matcher(text);
        int count = 0;
        while (matcher.find()) {
            count++;
        }
        return count;
    }

}
