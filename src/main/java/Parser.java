package plc.project;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The parser takes the sequence of tokens emitted by the lexer and turns that
 * into a structured representation of the program, called the Abstract Syntax
 * Tree (AST).
 *
 * The parser has a similar architecture to the lexer, just with {@link Token}s
 * instead of characters. As before, {@link #peek(Object...)} and {@link
 * #match(Object...)} are helpers to make the implementation easier.
 *
 * This type of parser is called <em>recursive descent</em>. Each rule in our
 * grammar will have it's own function, and reference to other rules correspond
 * to calling those functions.
 */
public final class Parser {

    private final TokenStream tokens;

    public Parser(List<Token> tokens) {
        this.tokens = new TokenStream(tokens);
    }

    /**
     * Parses the {@code source} rule.
     */
    public Ast.Source parseSource() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code equality-expression} rule.
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        Ast.Expression receiver = parsePrimaryExpression();

        while (peek(".")) {
            tokens.advance();
            if (!peek(Token.Type.IDENTIFIER)) {
                throw new ParseException("Expected member name.", getErrorIndex());
            }
            String memberName = tokens.get(0).getLiteral();
            tokens.advance();

            if (peek("(")) {
                tokens.advance();
                List<Ast.Expression> args = parseArgList();
                requireToken(")");
                receiver = new Ast.Expression.Function(Optional.of(receiver), memberName, args);
            } else {
                receiver = new Ast.Expression.Access(Optional.of(receiver), memberName);
            }
        }

        return receiver;
    }

    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
        if (peek("NIL"))   { tokens.advance(); return new Ast.Expression.Literal(null); }
        if (peek("TRUE"))  { tokens.advance(); return new Ast.Expression.Literal(Boolean.TRUE); }
        if (peek("FALSE")) { tokens.advance(); return new Ast.Expression.Literal(Boolean.FALSE); }

        if (peek(Token.Type.INTEGER)) {
            String raw = tokens.get(0).getLiteral();
            tokens.advance();
            return new Ast.Expression.Literal(new BigInteger(raw));
        }

        if (peek(Token.Type.DECIMAL)) {
            String raw = tokens.get(0).getLiteral();
            tokens.advance();
            return new Ast.Expression.Literal(new BigDecimal(raw));
        }

        if (peek(Token.Type.CHARACTER)) {
            String quoted = tokens.get(0).getLiteral();
            tokens.advance();
            char ch = resolveEscapes(quoted.substring(1, quoted.length() - 1)).charAt(0);
            return new Ast.Expression.Literal(ch);
        }

        if (peek(Token.Type.STRING)) {
            String quoted = tokens.get(0).getLiteral();
            tokens.advance();
            String content = resolveEscapes(quoted.substring(1, quoted.length() - 1));
            return new Ast.Expression.Literal(content);
        }

        if (peek("(")) {
            tokens.advance();
            Ast.Expression grouped = parseExpression();
            requireToken(")");
            return new Ast.Expression.Group(grouped);
        }

        if (peek(Token.Type.IDENTIFIER)) {
            String ident = tokens.get(0).getLiteral();
            tokens.advance();
            if (peek("(")) {
                tokens.advance();
                List<Ast.Expression> args = parseArgList();
                requireToken(")");
                return new Ast.Expression.Function(Optional.empty(), ident, args);
            }
            return new Ast.Expression.Access(Optional.empty(), ident);
        }

        throw new ParseException("Expected an expression.", getErrorIndex());
    }

    // parses comma-separated expressions until ')'
    private List<Ast.Expression> parseArgList() throws ParseException {
        List<Ast.Expression> args = new ArrayList<>();
        if (!peek(")")) {
            args.add(parseExpression());
            while (peek(",")) {
                tokens.advance();
                args.add(parseExpression());
            }
        }
        return args;
    }

    // replaces escape sequences in an unquoted string body
    private String resolveEscapes(String raw) {
        StringBuilder out = new StringBuilder(raw.length());
        int pos = 0;
        while (pos < raw.length()) {
            char ch = raw.charAt(pos);
            if (ch == '\\' && pos + 1 < raw.length()) {
                char next = raw.charAt(pos + 1);
                switch (next) {
                    case 'b':  out.append('\b'); break;
                    case 'n':  out.append('\n'); break;
                    case 'r':  out.append('\r'); break;
                    case 't':  out.append('\t'); break;
                    case '\'': out.append('\''); break;
                    case '"':  out.append('"');  break;
                    case '\\': out.append('\\'); break;
                    default:   out.append('\\'); out.append(next); break;
                }
                pos += 2;
            } else {
                out.append(ch);
                pos++;
            }
        }
        return out.toString();
    }

    // returns the index for error reporting — current token, or just past the last
    private int getErrorIndex() {
        if (tokens.has(0)) return tokens.get(0).getIndex();
        Token last = tokens.get(-1);
        return last.getIndex() + last.getLiteral().length();
    }

    // requires the current token to match expected literal, then advances
    private void requireToken(String expected) throws ParseException {
        if (!peek(expected)) {
            throw new ParseException("Expected '" + expected + "'.", getErrorIndex());
        }
        tokens.advance();
    }

    /**
     * As in the lexer, returns {@code true} if the current sequence of tokens
     * matches the given patterns. Unlike the lexer, the pattern is not a regex;
     * instead it is either a {@link Token.Type}, which matches if the token's
     * type is the same, or a {@link String}, which matches if the token's
     * literal is the same.
     *
     * In other words, {@code Token(IDENTIFIER, "literal")} is matched by both
     * {@code peek(Token.Type.IDENTIFIER)} and {@code peek("literal")}.
     */
    private boolean peek(Object... patterns) {
        for (int offset = 0; offset < patterns.length; offset++) {
            if (!tokens.has(offset)) return false;
            Token candidate = tokens.get(offset);
            Object pattern = patterns[offset];
            if (pattern instanceof Token.Type) {
                if (candidate.getType() != pattern) return false;
            } else if (pattern instanceof String) {
                if (!candidate.getLiteral().equals(pattern)) return false;
            } else {
                throw new AssertionError("Unsupported pattern type: " + pattern.getClass());
            }
        }
        return true;
    }

    /**
     * As in the lexer, returns {@code true} if {@link #peek(Object...)} is true
     * and advances the token stream.
     */
    private boolean match(Object... patterns) {
        if (!peek(patterns)) return false;
        for (int i = 0; i < patterns.length; i++) tokens.advance();
        return true;
    }

    private static final class TokenStream {

        private final List<Token> tokens;
        private int index = 0;

        private TokenStream(List<Token> tokens) {
            this.tokens = tokens;
        }

        /**
         * Returns true if there is a token at index + offset.
         */
        public boolean has(int offset) {
            int target = index + offset;
            return target >= 0 && target < tokens.size();
        }

        /**
         * Gets the token at index + offset.
         */
        public Token get(int offset) {
            return tokens.get(index + offset);
        }

        /**
         * Advances to the next token, incrementing the index.
         */
        public void advance() {
            index++;
        }

    }

}
