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
        List<Ast.Field> fields = new ArrayList<>();
        List<Ast.Method> methods = new ArrayList<>();

        while (tokens.has(0)) {
            if (peek("LET"))
                fields.add(parseField());
            else if (peek("DEF"))
                methods.add(parseMethod());
            else
                throw new ParseException("Expected LET or DEF.", errIdx());
        }

        return new Ast.Source(fields, methods);
    }

    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        match("LET");
        boolean isConst = match("CONST");

        if (!peek(Token.Type.IDENTIFIER))
            throw new ParseException("Expected field name.", errIdx());
        String name = tokens.get(0).getLiteral();
        tokens.advance();

        Optional<Ast.Expression> val = Optional.empty();
        if (match("="))
            val = Optional.of(parseExpression());

        reqTok(";");
        return new Ast.Field(name, isConst, val);
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        match("DEF");

        if (!peek(Token.Type.IDENTIFIER))
            throw new ParseException("Expected method name.", errIdx());
        String name = tokens.get(0).getLiteral();
        tokens.advance();

        reqTok("(");
        List<String> params = new ArrayList<>();
        if (peek(Token.Type.IDENTIFIER)) {
            params.add(tokens.get(0).getLiteral());
            tokens.advance();
            while (match(",")) {
                if (!peek(Token.Type.IDENTIFIER))
                    throw new ParseException("Expected parameter name.", errIdx());
                params.add(tokens.get(0).getLiteral());
                tokens.advance();
            }
        }
        reqTok(")");
        reqTok("DO");

        List<Ast.Statement> body = new ArrayList<>();
        while (tokens.has(0) && !peek("END"))
            body.add(parseStatement());

        reqTok("END");
        return new Ast.Method(name, params, body);
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        if (peek("LET"))    return parseDeclarationStatement();
        if (peek("IF"))     return parseIfStatement();
        if (peek("FOR"))    return parseForStatement();
        if (peek("WHILE"))  return parseWhileStatement();
        if (peek("RETURN")) return parseReturnStatement();

        // expression or assignment
        Ast.Expression lhs = parseExpression();
        if (match("=")) {
            Ast.Expression rhs = parseExpression();
            reqTok(";");
            return new Ast.Statement.Assignment(lhs, rhs);
        }
        reqTok(";");
        return new Ast.Statement.Expression(lhs);
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
        match("LET");
        if (!peek(Token.Type.IDENTIFIER))
            throw new ParseException("Expected variable name.", errIdx());
        String varName = tokens.get(0).getLiteral();
        tokens.advance();

        Optional<Ast.Expression> val = Optional.empty();
        if (match("="))
            val = Optional.of(parseExpression());

        reqTok(";");
        return new Ast.Statement.Declaration(varName, val);
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        match("IF");
        Ast.Expression cond = parseExpression();
        reqTok("DO");

        List<Ast.Statement> thenStmts = new ArrayList<>();
        while (tokens.has(0) && !peek("ELSE") && !peek("END"))
            thenStmts.add(parseStatement());

        List<Ast.Statement> elseStmts = new ArrayList<>();
        if (match("ELSE")) {
            while (tokens.has(0) && !peek("END"))
                elseStmts.add(parseStatement());
        }

        reqTok("END");
        return new Ast.Statement.If(cond, thenStmts, elseStmts);
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        match("FOR");
        reqTok("(");

        Ast.Statement init = null;
        if (peek(Token.Type.IDENTIFIER) && tokens.has(1) && tokens.get(1).getLiteral().equals("=")) {
            String varName = tokens.get(0).getLiteral();
            tokens.advance();
            tokens.advance(); // consume '='
            init = new Ast.Statement.Assignment(
                    new Ast.Expression.Access(Optional.empty(), varName),
                    parseExpression()
            );
        }
        reqTok(";");

        Ast.Expression cond = parseExpression();
        reqTok(";");

        Ast.Statement step = null;
        if (peek(Token.Type.IDENTIFIER) && tokens.has(1) && tokens.get(1).getLiteral().equals("=")) {
            String varName = tokens.get(0).getLiteral();
            tokens.advance();
            tokens.advance(); // consume '='
            step = new Ast.Statement.Assignment(
                    new Ast.Expression.Access(Optional.empty(), varName),
                    parseExpression()
            );
        }
        reqTok(")");

        List<Ast.Statement> body = new ArrayList<>();
        while (tokens.has(0) && !peek("END"))
            body.add(parseStatement());

        reqTok("END");
        return new Ast.Statement.For(init, cond, step, body);
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        match("WHILE");
        Ast.Expression cond = parseExpression();
        reqTok("DO");

        List<Ast.Statement> body = new ArrayList<>();
        while (tokens.has(0) && !peek("END"))
            body.add(parseStatement());

        reqTok("END");
        return new Ast.Statement.While(cond, body);
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        match("RETURN");
        Ast.Expression val = parseExpression();
        reqTok(";");
        return new Ast.Statement.Return(val);
    }

    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
        return parseLogicalExpression();
    }

    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        Ast.Expression lhs = parseEqualityExpression();
        while (peek("&&") || peek("||")) {
            String op = tokens.get(0).getLiteral();
            tokens.advance();
            lhs = new Ast.Expression.Binary(op, lhs, parseEqualityExpression());
        }
        return lhs;
    }

    /**
     * Parses the {@code equality-expression} rule.
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
        Ast.Expression lhs = parseAdditiveExpression();
        String op = peek(Token.Type.OPERATOR) ? tokens.get(0).getLiteral() : "";
        while (op.equals("<") || op.equals("<=") || op.equals(">") || op.equals(">=")
                || op.equals("==") || op.equals("!=")) {
            tokens.advance();
            lhs = new Ast.Expression.Binary(op, lhs, parseAdditiveExpression());
            op = peek(Token.Type.OPERATOR) ? tokens.get(0).getLiteral() : "";
        }
        return lhs;
    }

    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        Ast.Expression lhs = parseMultiplicativeExpression();
        while (peek("+") || peek("-")) {
            String op = tokens.get(0).getLiteral();
            tokens.advance();
            lhs = new Ast.Expression.Binary(op, lhs, parseMultiplicativeExpression());
        }
        return lhs;
    }

    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        Ast.Expression lhs = parseSecondaryExpression();
        while (peek("*") || peek("/")) {
            String op = tokens.get(0).getLiteral();
            tokens.advance();
            lhs = new Ast.Expression.Binary(op, lhs, parseSecondaryExpression());
        }
        return lhs;
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        Ast.Expression rcvr = parsePrimaryExpression();

        while (match(".")) {
            if (!peek(Token.Type.IDENTIFIER))
                throw new ParseException("Expected member name after '.'", errIdx());

            String mem = tokens.get(0).getLiteral();
            tokens.advance();

            if (match("(")) {
                List<Ast.Expression> args = new ArrayList<>();
                if (peek(")")) {
                    reqTok(")");
                    rcvr = new Ast.Expression.Function(Optional.of(rcvr), mem, args);
                    continue;
                }
                args.add(parseExpression());
                while (match(","))
                    args.add(parseExpression());
                reqTok(")");
                rcvr = new Ast.Expression.Function(Optional.of(rcvr), mem, args);
            } else {
                rcvr = new Ast.Expression.Access(Optional.of(rcvr), mem);
            }
        }

        return rcvr;
    }

    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
        if (match("NIL"))   return new Ast.Expression.Literal(null);
        if (match("TRUE"))  return new Ast.Expression.Literal(Boolean.TRUE);
        if (match("FALSE")) return new Ast.Expression.Literal(Boolean.FALSE);

        if (peek(Token.Type.INTEGER)) {
            BigInteger val = new BigInteger(tokens.get(0).getLiteral());
            tokens.advance();
            return new Ast.Expression.Literal(val);
        }

        if (peek(Token.Type.DECIMAL)) {
            BigDecimal val = new BigDecimal(tokens.get(0).getLiteral());
            tokens.advance();
            return new Ast.Expression.Literal(val);
        }

        if (peek(Token.Type.CHARACTER)) {
            String stripped = tokens.get(0).getLiteral();
            tokens.advance();
            stripped = stripped.substring(1, stripped.length() - 1);
            StringBuilder sb = new StringBuilder();
            int i = 0;
            while (i < stripped.length()) {
                if (stripped.charAt(i) == '\\') {
                    char nx = stripped.charAt(i + 1);
                    if      (nx == 'n')  sb.append('\n');
                    else if (nx == 't')  sb.append('\t');
                    else if (nx == 'r')  sb.append('\r');
                    else if (nx == 'b')  sb.append('\b');
                    else if (nx == '\'') sb.append('\'');
                    else if (nx == '"')  sb.append('"');
                    else if (nx == '\\') sb.append('\\');
                    else { sb.append('\\'); sb.append(nx); }
                    i += 2;
                } else {
                    sb.append(stripped.charAt(i));
                    i++;
                }
            }
            return new Ast.Expression.Literal(sb.toString().charAt(0));
        }

        if (peek(Token.Type.STRING)) {
            String stripped = tokens.get(0).getLiteral();
            tokens.advance();
            stripped = stripped.substring(1, stripped.length() - 1);
            StringBuilder sb = new StringBuilder();
            int i = 0;
            while (i < stripped.length()) {
                if (stripped.charAt(i) == '\\') {
                    char nx = stripped.charAt(i + 1);
                    if      (nx == 'n')  sb.append('\n');
                    else if (nx == 't')  sb.append('\t');
                    else if (nx == 'r')  sb.append('\r');
                    else if (nx == 'b')  sb.append('\b');
                    else if (nx == '\'') sb.append('\'');
                    else if (nx == '"')  sb.append('"');
                    else if (nx == '\\') sb.append('\\');
                    else { sb.append('\\'); sb.append(nx); }
                    i += 2;
                } else {
                    sb.append(stripped.charAt(i));
                    i++;
                }
            }
            return new Ast.Expression.Literal(sb.toString());
        }

        if (match("(")) {
            Ast.Expression grouped = parseExpression();
            reqTok(")");
            return new Ast.Expression.Group(grouped);
        }

        if (peek(Token.Type.IDENTIFIER)) {
            String name = tokens.get(0).getLiteral();
            tokens.advance();
            if (!match("("))
                return new Ast.Expression.Access(Optional.empty(), name);
            List<Ast.Expression> args = new ArrayList<>();
            if (!peek(")")) {
                args.add(parseExpression());
                while (match(","))
                    args.add(parseExpression());
            }
            reqTok(")");
            return new Ast.Expression.Function(Optional.empty(), name, args);
        }

        throw new ParseException("Expected an expression.", errIdx());
    }

    // current token index for errors, or end of last token if stream is done
    private int errIdx() {
        if (tokens.has(0))
            return tokens.get(0).getIndex();
        if (!tokens.has(-1))
            return 0;
        Token last = tokens.get(-1);
        return last.getIndex() + last.getLiteral().length();
    }

    // consumes expected token or throws
    private void reqTok(String expected) throws ParseException {
        if (!match(expected))
            throw new ParseException("Expected '" + expected + "'.", errIdx());
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
        for (int i = 0; i < patterns.length; i++) {
            if (!tokens.has(i)) return false;
            Token t = tokens.get(i);
            Object p = patterns[i];
            if (p instanceof Token.Type) {
                if (t.getType() != p) return false;
            } else if (p instanceof String) {
                if (!t.getLiteral().equals(p)) return false;
            } else {
                throw new AssertionError("Unsupported pattern type: " + p.getClass());
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
