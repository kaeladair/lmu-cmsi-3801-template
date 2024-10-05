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
    static long meaningfulLineCount(String filename) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            return reader.lines()
                    .filter(line -> !line.trim().isEmpty() && !line.trim().startsWith("#"))
                    .count();
        }
    }
}

// Write your Quaternion record class here
record Quaternion(double a, double b, double c, double d) {
    public Quaternion {
        if (Double.isNaN(a) || Double.isNaN(b) || Double.isNaN(c) || Double.isNaN(d)) {
            throw new IllegalArgumentException("Coefficients cannot be NaN");
        }
    }

    public static final Quaternion ZERO = new Quaternion(0, 0, 0, 0);
    public static final Quaternion I = new Quaternion(0, 1, 0, 0);
    public static final Quaternion J = new Quaternion(0, 0, 1, 0);
    public static final Quaternion K = new Quaternion(0, 0, 0, 1);

    public List<Double> coefficients() {
        return List.of(a, b, c, d);
    }

    public Quaternion conjugate() {
        return new Quaternion(a, -b, -c, -d);
    }

    public Quaternion plus(Quaternion q) {
        return new Quaternion(a + q.a, b + q.b, c + q.c, d + q.d);
    }

    public Quaternion times(Quaternion q) {
        return new Quaternion(
            a * q.a - b * q.b - c * q.c - d * q.d,
            a * q.b + b * q.a + c * q.d - d * q.c,
            a * q.c - b * q.d + c * q.a + d * q.b,
            a * q.d + b * q.c - c * q.b + d * q.a
        );
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        if (a != 0 || (b == 0 && c == 0 && d == 0)) {
            sb.append(a);
            first = false;
        }
        if (b != 0) {
            sb.append(first ? b : (b > 0 ? "+" + b : b)).append("i");
            first = false;
        }
        if (c != 0) {
            sb.append(first ? c : (c > 0 ? "+" + c : c)).append("j");
            first = false;
        }
        if (d != 0) {
            sb.append(first ? d : (d > 0 ? "+" + d : d)).append("k");
        }
        return sb.toString();
    }
}

// Write your BinarySearchTree sealed interface and its implementations here
