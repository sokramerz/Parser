import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Contains JUnit tests for {@link Regex}. A framework of the test structure 
 * is provided, you will fill in the remaining pieces.
 *
 * To run tests, either click the run icon on the left margin, which can be used
 * to run all tests or only a specific test. You should make sure your tests are
 * run through IntelliJ (File > Settings > Build, Execution, Deployment > Build
 * Tools > Gradle > Run tests using <em>IntelliJ IDEA</em>). This ensures the
 * name and inputs for the tests are displayed correctly in the run window.
 */
public class RegexTests {

    /**
     * This is a parameterized test for the {@link Regex#EMAIL} regex. The
     * {@link ParameterizedTest} annotation defines this method as a
     * parameterized test, and {@link MethodSource} tells JUnit to look for the
     * static method {@link #testEmailRegex()}.
     *
     * For personal preference, I include a test name as the first parameter
     * which describes what that test should be testing - this is visible in
     * IntelliJ when running the tests (see above note if not working).
     */
    @ParameterizedTest
    @MethodSource
    public void testEmailRegex(String test, String input, boolean success) {
        test(input, Regex.EMAIL, success);
    }

    /**
     * This is the factory method providing test cases for the parameterized
     * test above - note that it is static, takes no arguments, and has the same
     * name as the test. The {@link Arguments} object contains the arguments for
     * each test to be passed to the function above.
     */
    public static Stream<Arguments> testEmailRegex() {
        return Stream.of(
                Arguments.of("Alphanumeric", "thelegend27@gmail.com", true),
                Arguments.of("UF Domain", "otherdomain@ufl.edu", true),
                Arguments.of("Missing Domain Dot", "missingdot@gmailcom", false),
                Arguments.of("Symbols", "symbols#$%@gmail.com", false),

                // Matching cases new tests
                Arguments.of("Top-Level Domain Length", "thepresident@gmail.xi", true),
                Arguments.of("Numeric", "747747@fly.com", true),
                Arguments.of("Special Char", "Hello-World.Its.Me@hotmail.com", true),

                // Remaining 3 false tests
                Arguments.of("Top-Level Domain Length", "otherdomain@ufl.info", false),
                Arguments.of("Domain", "Idonotlikejava@UFL_csc.edu", false),
                Arguments.of("Top-Level Domain Cap", "itssofree@gmail.COM", false),

                // Extra Tests to cover all
                Arguments.of("No @", "feelgoodgmail.com", false),
                Arguments.of("Special Char", "heyitsme!#@gmail.com", false),
                Arguments.of("Domain .", "dontforgetperiod@gmailcom", false),
                Arguments.of("Domain Spec Char", "Ilovejava@ufl-edu.csc", true)
        );
    }

    @ParameterizedTest
    @MethodSource
    public void testOddStringsRegex(String test, String input, boolean success) {
        test(input, Regex.ODD_STRINGS, success);
    }

    public static Stream<Arguments> testOddStringsRegex() {
        return Stream.of(
                // what have eleven letters and starts with gas?
                Arguments.of("11 Characters", "automobiles", true),
                Arguments.of("13 Characters", "i<3pancakes13", true),
                Arguments.of("5 Characters", "5five", false),
                Arguments.of("14 Characters", "i<3pancakes14!", false),

                // new 5 passing
                Arguments.of("15 Char symbol", "!!!!!@@@@@#####", true),
                Arguments.of("17 Chars w/spaces", "I don't like 1234", true),
                Arguments.of("19 chars", "1111222233334444555", true),
                Arguments.of("11 chars white space", "           ", true),
                Arguments.of("15 chars spaced start", " 123456789012345678", true),

                // new 5 failing
                Arguments.of("Too long", "1145-908175-4198714-58914589", false),
                Arguments.of("12 char even", "123456789012", false),
                Arguments.of("Too short", "123456789", false),
                Arguments.of("20 char even max", "12345678901234567890", false),
                Arguments.of("10 char even", "1234567890", false)
        );
    }

    @ParameterizedTest
    @MethodSource
    public void testIntegerListRegex(String test, String input, boolean success) {
        test(input, Regex.INTEGER_LIST, success);
    }

    public static Stream<Arguments> testIntegerListRegex() {
        return Stream.of(
                Arguments.of("Single Element", "[1]", true),
                Arguments.of("Multiple Elements", "[1,20,3]", true),
                Arguments.of("Missing Brackets", "1,2,3", false),
                Arguments.of("Missing Commas", "[1 2 3]", false),

                // new true test cases
                Arguments.of("std spacing", "[1, 2, 3, 4, 5]", true),
                Arguments.of("Different spacing","[1, 2,3,4, 5]", true),
                Arguments.of("Empty list","[]", true),
                Arguments.of("Positive","[123456789]", true),
                Arguments.of("Two elemnts","[1,2]", true),

                // new false test cases
                Arguments.of("Negative Num","[-1]", false),
                Arguments.of("Zero","[0]", false),
                Arguments.of("Multiple spaces","[1,2,    5]", false),
                Arguments.of("Trailing comma","[1, 2, 4,]", false),
                Arguments.of("Leading Zero","[01, 02, 03]", false)
        );
    }

    @ParameterizedTest
    @MethodSource
    public void testDecimalRegex(String test, String input, boolean success) {
        test(input, Regex.DECIMAL, success);
    }

    public static Stream<Arguments> testDecimalRegex() {
        return Stream.of(
                // true cases
                Arguments.of("Random pos decimal", "482.291", true),
                Arguments.of("Negative zero start", "-0.0001", true),
                Arguments.of("Trailing zeros", "10.50000", true),
                Arguments.of("Large negative", "-9921.1", true),
                Arguments.of("Zero", "0.0", true),
                Arguments.of("Tiny number", "0.00001", true),

                // false cases
                Arguments.of("Leading zero fail", "05.50", false),
                Arguments.of("No decimal part", "125.", false),
                Arguments.of("Missing int part", ".88", false),
                Arguments.of("Double . typo", "5..2", false),
                Arguments.of("Integer only", "42", false),
                Arguments.of("Sign but no num", "-.5", false)
        );
    }

    @ParameterizedTest
    @MethodSource
    public void testStringRegex(String test, String input, boolean success) {
        test(input, Regex.STRING, success);
    }

    public static Stream<Arguments> testStringRegex() {
        return Stream.of(
                // true cases
                Arguments.of("Standard string", "\"I've heard it both ways\"", true),
                Arguments.of("Escaped quotes", "\"\\\"The Jackal\\\" arrived\"", true),
                Arguments.of("Escaped tab", "\"C'mon\\tSon\"", true),
                Arguments.of("Empty string", "\"\"", true),
                Arguments.of("Escaped slash", "\"P:\\\\Psych\\\\Office\"", true),

                // false cases
                Arguments.of("Unterminated", "\"Did you hear about Pluto?", false),
                Arguments.of("Invalid escape", "\"Pineapple\\pizza\"", false),
                Arguments.of("Unescaped inner quote", "\"Gus \"Silly\" Name\"", false),
                Arguments.of("No start quote", "Lassiter\"", false),
                Arguments.of("Trailing escape", "\"Wait for iiiit\\\"", false)
        );
    }

    /**
     * Asserts that the input matches the given pattern. This method doesn't do
     * much now, but you will see this concept in future assignments.
     */
    private static void test(String input, Pattern pattern, boolean success) {
        Assertions.assertEquals(success, pattern.matcher(input).matches());
    }

}