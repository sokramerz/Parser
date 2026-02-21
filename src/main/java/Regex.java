import java.util.regex.Pattern;

/**
 * Contains {@link Pattern} constants, which are compiled regular expressions.
 * See the assignment page for resources on regexes as needed.
 */
public class Regex {

    public static final Pattern
            EMAIL = Pattern.compile("[A-Za-z0-9._-]+@[A-Za-z0-9-]*\\.[a-z]{2,3}"),
            ODD_STRINGS = Pattern.compile(".{11}|.{13}|.{15}|.{17}|.{19}"),   // Straightforward solution

            // comma separated, POSITIVE int, surrounded by square bracket. option space after comma
            INTEGER_LIST = Pattern.compile("\\[([1-9][0-9]*(, ?[1-9][0-9]*)*)?\\]"),

            // dec, two int, both int present, separated by a dec point. non-zero start. optional -. split it up
            DECIMAL = Pattern.compile("-?0\\.[0-9]+|-?[1-9][0-9]*\\.[0-9]+"),

            // double quotedstring, esc with "\", followed by one brnt'"\.
            STRING = Pattern.compile("\"([^\\\\\"]|\\\\[bnrt'\"\\\\])*\"");

}
