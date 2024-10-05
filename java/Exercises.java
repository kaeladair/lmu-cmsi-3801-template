import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;
import java.util.function.Predicate;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Exercises {
    static Map<Integer, Long> change(long amount) {
        if (amount < 0) {
            throw new IllegalArgumentException("Amount cannot be negative");
        }
        var counts = new HashMap<Integer, Long>();
        for (var denomination : List.of(25, 10, 5, 1)) {
            counts.put(denomination, amount / denomination);
            amount %= denomination;
        }
        return counts;
    }

    // Write your first then lower case function here
    static Optional<String> firstThenLowerCase(List<String> strings, Predicate<String> predicate) {
        return strings.stream()
            .filter(predicate)
            .findFirst()
            .map(String::toLowerCase);
    }

    // Write your say function here
    static class Say {
        private final StringBuilder phrase = new StringBuilder();

        private Say(String word) {
            if (word != null && !word.isEmpty()) {
                phrase.append(word);
            }
        }

        public Say and(String word) {
            if (word != null && !word.isEmpty()) {
                if (phrase.length() > 0) {
                    phrase.append(" ");
                }
                phrase.append(word);
            }
            return this;
        }

        public String phrase() {
            return phrase.toString();
        }

        public static Say say(String word) {
            return new Say(word);
        }
    }

    // Write your line count function here
}

// Write your Quaternion record class here

// Write your BinarySearchTree sealed interface and its implementations here
