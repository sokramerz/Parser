package plc.project;

import java.util.ArrayList;
import java.util.List;

/**
 * The lexer works through three main functions:
 *
 * - {@link #lex()}, which repeatedly calls lexToken() and skips whitespace
 * - {@link #lexToken()}, which lexes the next token
 * - {@link CharStream}, which manages the state of the lexer and literals
 *
 * If the lexer fails to parse something (such as an unterminated string) you
 * should throw a {@link ParseException} with an index at the invalid character.
 *
 * The {@link #peek(String...)} and {@link #match(String...)} functions are
 * helpers you need to use, they will make the implementation easier.
 */
public final class Lexer {

    private final CharStream chars;

    public Lexer(String input) {
        chars = new CharStream(input);
    }

    /**
     * Repeatedly lexes the input using {@link #lexToken()}, also skipping over
     * whitespace where appropriate.
     */
    public List<Token> lex() {
        List<Token> tokens = new ArrayList<>();
        while (chars.has(0)) {
            if (match("[ \\n\\r\\t]")) {
                chars.skip();
            } else {
                tokens.add(lexToken());
            }
        }
        return tokens;
    }

    /**
     * This method determines the type of the next token, delegating to the
     * appropriate lex method. As such, it is best for this method to not change
     * the state of the char stream (thus, use peek not match).
     *
     * The next character should start a valid token since whitespace is handled
     * by {@link #lex()}
     */
    public Token lexToken() {
        char c = chars.get(0);
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_') {
            return lexIdentifier();
        } else if ((c >= '0' && c <= '9') || (c == '-' && chars.has(1) && chars.get(1) >= '0' && chars.get(1) <= '9')) {
            return lexNumber();
        } else if (c == '\'') {
            return lexCharacter();
        } else if (c == '"') {
            return lexString();
        } else {
            return lexOperator();
        }
    }

    public Token lexIdentifier() {
        match("[A-Za-z_]");
        while (match("[A-Za-z0-9_-]"));
        return chars.emit(Token.Type.IDENTIFIER);
    }

    public Token lexNumber() {
        if (peek("-")) {
            match("-");
        } else if (peek("+")) {
            match("+");
        }

        if (peek("0")) {
            match("0");
            if (peek(".", "[0-9]")) {
                match(".");
                while (peek("[0-9]")) {
                    match("[0-9]");
                }
                return chars.emit(Token.Type.DECIMAL);
            }
            return chars.emit(Token.Type.INTEGER);
        }

        while (peek("[0-9]")) {
            match("[0-9]");
        }

        if (peek(".", "[0-9]")) {
            match(".");
            while (peek("[0-9]")) {
                match("[0-9]");
            }
            return chars.emit(Token.Type.DECIMAL);
        }

        return chars.emit(Token.Type.INTEGER);
    }

    public Token lexCharacter() {
        match("'");

        if (peek("'")) {
            throw new ParseException("Missing character", chars.index);
        }
        
        if (peek("\\")) {
            lexEscape();
        } else if (peek("\n") || peek("\r")) {
             throw new ParseException("Illegal character", chars.index);
        } else {
             match(".");
        }
        
        if (peek("'")) {
            match("'");
            return chars.emit(Token.Type.CHARACTER);
        }
        
        throw new ParseException("Unfin character", chars.index);
    }

    public Token lexString() {
        match("\"");
        while (!peek("\"")) {
            if (peek("\\\\")) { 
                lexEscape();
            } else if (peek("[^\"\\n\\r\\\\]")) {
                match(".");
            } else {
                throw new ParseException("Unfin stirng", chars.index);
            }
        }
        match("\"");
        return chars.emit(Token.Type.STRING);
    }

    public void lexEscape() {
        match("\\\\"); 
        if (!match("[bnrt'\"\\\\]")) {
            throw new ParseException("Invalid escape", chars.index);
        }
    }

    public Token lexOperator() {
        if (peek("!", "=") || peek("=", "=") || peek("<", "=") || peek(">", "=")) {
            match(".", ".");
        } else {
            match(".");
        }
        return chars.emit(Token.Type.OPERATOR);
    }

    /**
     * Returns true if the next sequence of characters match the given patterns,
     * which should be a regex. For example, {@code peek("a", "b", "c")} would
     * return true if the next characters are {@code 'a', 'b', 'c'}.
     */
    public boolean peek(String... patterns) {
        for (int offset = 0; offset < patterns.length; offset++) {
            if (!chars.has(offset) || !String.valueOf(chars.get(offset)).matches(patterns[offset])) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns true in the same way as {@link #peek(String...)}, but also
     * advances the character stream past all matched characters if peek returns
     * true. Hint - it's easiest to have this method simply call peek.
     */
    public boolean match(String... patterns) {
        boolean matches = peek(patterns);
        if (matches) {
            for (int i = 0; i < patterns.length; i++) {
                chars.advance();
            }
        }
        return matches;
    }

    /**
     * A helper class maintaining the input string, current index of the char
     * stream, and the current length of the token being matched.
     *
     * You should rely on peek/match for state management in nearly all cases.
     * The only field you need to access is {@link #index} for any {@link
     * ParseException} which is thrown.
     */
    public static final class CharStream {

        private final String input;
        private int index = 0;
        private int length = 0;

        public CharStream(String input) {
            this.input = input;
        }

        public boolean has(int offset) {
            return index + offset < input.length();
        }

        public char get(int offset) {
            return input.charAt(index + offset);
        }

        public void advance() {
            index++;
            length++;
        }

        public void skip() {
            length = 0;
        }

        public Token emit(Token.Type type) {
            int start = index - length;
            skip();
            return new Token(type, input.substring(start, index), start);
        }

    }

}
