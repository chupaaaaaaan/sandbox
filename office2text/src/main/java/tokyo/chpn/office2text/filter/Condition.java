package tokyo.chpn.office2text.filter;

import java.util.StringJoiner;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public record Condition(String description, Predicate<String> predicate) {

    public static Condition createCaseInsensitiveWordMatchCondition(String description, String word, String... otherExcludePattern) {
        Pattern pattern1 = Pattern.compile(Pattern.quote(word), Pattern.CASE_INSENSITIVE);
        Pattern pattern2 = Pattern.compile(makeExcludePattern(word, otherExcludePattern), Pattern.CASE_INSENSITIVE);

        return new Condition(description, l -> countMatches(pattern1, l) != countMatches(pattern2, l));
    }

    public static Condition createMatchCondition(String description, String word, String... otherExcludePatterns) {
        Pattern pattern1 = Pattern.compile(Pattern.quote(word));
        Pattern pattern2 = Pattern.compile(makeExcludePattern(word, otherExcludePatterns));

        return new Condition(description, l -> countMatches(pattern1, l) != countMatches(pattern2, l));
    }

    private static String makeExcludePattern(String word, String... otherExcludePatterns) {
        StringJoiner excludePattern = new StringJoiner("|");
        excludePattern.add("\\w" + Pattern.quote(word));
        excludePattern.add(Pattern.quote(word) + "\\w");
        for (String pattern : otherExcludePatterns) {
            excludePattern.add(pattern);
        }
        return excludePattern.toString();
    }

    public static Condition createPhraseMatchCondition(String description, String phrase) {
        Pattern pattern = Pattern.compile(Pattern.quote(phrase));
        return new Condition(description, l -> pattern.matcher(l).matches());
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
