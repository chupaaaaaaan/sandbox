package tokyo.chpn.office2text.transform;

import tokyo.chpn.office2text.unit.ProcessingUnit;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TextFilter {

    private final Set<Predicate<String>> conditions;


    public TextFilter(Set<Predicate<String>> conditions) {
        this.conditions = conditions;
    }

    private Set<Predicate<String>> constructConditions(String conditionFile) {

        try (Stream<String> lines = Files.lines(Path.of(conditionFile))) {
            return lines.map(l -> (Predicate<String>) (String t) -> t.toLowerCase().contains(l.toLowerCase())).collect(Collectors.toUnmodifiableSet());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public List<ProcessingUnit> search(List<ProcessingUnit> units) {

        List<ProcessingUnit> result = new ArrayList<>();

        for(ProcessingUnit u : units) {
            for(Predicate<String> condition : conditions) {
                if(condition.test(u.getUnit())) {
                    result.add(u);
                }
            }
        }
        return result;
    }
}
