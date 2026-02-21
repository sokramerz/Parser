package plc.project;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Converts a flat token stream produced by the Lexer into a hierarchical
 * Abstract Syntax Tree (AST) via recursive descent parsing. Each grammar
 * rule maps directly to a parse method; references to other rules become
 * calls to those methods.
 */
public final class Parser {

    private final TokenStream stream;

    public Parser(List<Token> tokens) {
        this.stream = new TokenStream(tokens);
    }

    // -------------------------------------------------------------------------
    // Top-level
    // -------------------------------------------------------------------------

    /**
     * Parses the {@code source} rule.
     * source ::= field* method*
     *
     * Loops over the token stream, routing LET tokens to parseField()
     * and DEF tokens to parseMethod(). Throws if neither is found.
     */
    public Ast.Source parseSource() throws ParseException {
        // TODO: build fieldList and methodList, loop while stream.has(0),
        //       dispatch on "LET" -> parseField(), "DEF" -> parseMethod(),
        //       throw ParseException for anything else.
        throw new UnsupportedOperationException("TODO: parseSource");
    }

    // -------------------------------------------------------------------------
    // Fields and Methods
    // -------------------------------------------------------------------------

    /**
     * Parses the {@code field} rule. Expects the current token to be {@code LET}.
     * field ::= 'LET' 'CONST'? identifier ('=' expression)? ';'
     *
     * Consume LET, optionally consume CONST (sets isConstant), read the field
     * name via expectIdentifier(), optionally parse '=' expression via
     * parseOptionalAssignment(), then requireSemicolon().
     */
    public Ast.Field parseField() throws ParseException {
        // TODO: consume("LET"), tryConsume("CONST"), expectIdentifier(),
        //       parseOptionalAssignment(), requireSemicolon().
        //       Return new Ast.Field(fieldName, isConstant, initializer).
        throw new UnsupportedOperationException("TODO: parseField");
    }

    /**
     * Parses the {@code method} rule. Expects the current token to be {@code DEF}.
     * method ::= 'DEF' identifier '(' (identifier (',' identifier)*)? ')' 'DO' statement* 'END'
     *
     * Consume DEF, read method name, require '(', parse identifier list for
     * params, require ')', require DO, collect statements until END, require END.
     */
    public Ast.Method parseMethod() throws ParseException {
        // TODO: consume("DEF"), expectIdentifier(), requireToken("("),
        //       parseIdentifierList(), requireToken(")"), requireToken("DO"),
        //       collectStatementsUntil("END"), requireToken("END").
        //       Return new Ast.Method(methodName, paramNames, body).
        throw new UnsupportedOperationException("TODO: parseMethod");
    }

    // -------------------------------------------------------------------------
    // Statements
    // -------------------------------------------------------------------------

    /**
     * Parses a single {@code statement}, dispatching to the appropriate
     * sub-parser based on the leading keyword. Falls back to an expression
     * or assignment statement if no keyword matches.
     *
     * statement ::= 'LET' ... | 'IF' ... | 'FOR' ... | 'WHILE' ... | 'RETURN' ...
     *             | expression ('=' expression)? ';'
     */
    public Ast.Statement parseStatement() throws ParseException {
        // TODO: chain of if(peek("LET")), if(peek("IF")), if(peek("FOR")),
        //       if(peek("WHILE")), if(peek("RETURN")), else parseExpressionOrAssignment().
        throw new UnsupportedOperationException("TODO: parseStatement");
    }

    /**
     * Parses a declaration statement.
     * 'LET' identifier ('=' expression)? ';'
     *
     * Consume LET, read variable name, optionally parse initializer, require ';'.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
        // TODO: consume("LET"), expectIdentifier("variable name"),
        //       parseOptionalAssignment(), requireSemicolon().
        //       Return new Ast.Statement.Declaration(varName, rhs).
        throw new UnsupportedOperationException("TODO: parseDeclarationStatement");
    }

    /**
     * Parses an if statement.
     * 'IF' expression 'DO' statement* ('ELSE' statement*)? 'END'
     *
     * Consume IF, parse condition expression, require DO, collect then-branch
     * statements until ELSE or END, optionally consume ELSE and collect
     * else-branch, require END.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        // TODO: consume("IF"), parseExpression(), requireToken("DO"),
        //       collectStatementsUntil("ELSE","END"),
        //       if tryConsume("ELSE") -> collectStatementsUntil("END"),
        //       requireToken("END").
        //       Return new Ast.Statement.If(guard, thenBranch, elseBranch).
        throw new UnsupportedOperationException("TODO: parseIfStatement");
    }

    /**
     * Parses a for statement.
     * 'FOR' '(' (identifier '=' expression)? ';' expression ';' (identifier '=' expression)? ')' statement* 'END'
     *
     * Consume FOR, require '(', optionally parse init assignment clause,
     * require first ';', parse loop condition, require second ';', optionally
     * parse step clause, require ')', collect body statements, require END.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        // TODO: consume("FOR"), requireToken("("),
        //       parseOptionalForAssignment(), requireToken(";"),
        //       parseExpression(), requireToken(";"),
        //       parseOptionalForAssignment(), requireToken(")"),
        //       collectStatementsUntil("END"), requireToken("END").
        //       Return new Ast.Statement.For(initClause, loopCondition, stepClause, loopBody).
        throw new UnsupportedOperationException("TODO: parseForStatement");
    }

    /**
     * Parses a while statement.
     * 'WHILE' expression 'DO' statement* 'END'
     *
     * Consume WHILE, parse condition, require DO, collect body, require END.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        // TODO: consume("WHILE"), parseExpression(), requireToken("DO"),
        //       collectStatementsUntil("END"), requireToken("END").
        //       Return new Ast.Statement.While(loopGuard, loopBody).
        throw new UnsupportedOperationException("TODO: parseWhileStatement");
    }

    /**
     * Parses a return statement.
     * 'RETURN' expression ';'
     *
     * Consume RETURN, parse the return value expression, require ';'.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        // TODO: consume("RETURN"), parseExpression(), requireSemicolon().
        //       Return new Ast.Statement.Return(returnVal).
        throw new UnsupportedOperationException("TODO: parseReturnStatement");
    }

    // -------------------------------------------------------------------------
    // Expressions — recursive descent by precedence (lowest -> highest)
    // -------------------------------------------------------------------------

    /**
     * Entry point for expression parsing; delegates immediately to the lowest
     * precedence level (logical).
     * expression ::= logical_expression
     */
    public Ast.Expression parseExpression() throws ParseException {
        return parseLogicalExpression();
    }

    /**
     * Parses a logical expression.
     * logical ::= equality (('&&' | '||') equality)*
     *
     * Left-associative: each iteration wraps the accumulating node on the left.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        Ast.Expression node = parseEqualityExpression();

        while (peek("&&") || peek("||")) {
            String op = stream.get(0).getLiteral();
            stream.advance();
            Ast.Expression rhs = parseEqualityExpression();
            node = new Ast.Expression.Binary(op, node, rhs);
        }

        return node;
    }

    /**
     * Parses a comparison/equality expression.
     * equality ::= additive (('<' | '<=' | '>' | '>=' | '==' | '!=') additive)*
     *
     * Uses atComparisonOperator() to check for any of the six operators.
     * Left-associative.
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
        Ast.Expression node = parseAdditiveExpression();

        while (atComparisonOperator()) {
            String op = stream.get(0).getLiteral();
            stream.advance();
            Ast.Expression rhs = parseAdditiveExpression();
            node = new Ast.Expression.Binary(op, node, rhs);
        }

        return node;
    }

    /**
     * Parses an additive expression.
     * additive ::= multiplicative (('+' | '-') multiplicative)*
     *
     * Left-associative.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        Ast.Expression node = parseMultiplicativeExpression();

        while (peek("+") || peek("-")) {
            String op = stream.get(0).getLiteral();
            stream.advance();
            Ast.Expression rhs = parseMultiplicativeExpression();
            node = new Ast.Expression.Binary(op, node, rhs);
        }

        return node;
    }

    /**
     * Parses a multiplicative expression.
     * multiplicative ::= secondary (('*' | '/') secondary)*
     *
     * Left-associative.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        Ast.Expression node = parseSecondaryExpression();

        while (peek("*") || peek("/")) {
            String op = stream.get(0).getLiteral();
            stream.advance();
            Ast.Expression rhs = parseSecondaryExpression();
            node = new Ast.Expression.Binary(op, node, rhs);
        }

        return node;
    }

    /**
     * Parses a secondary expression (member access / method calls).
     * secondary ::= primary ('.' identifier ('(' args ')')?)*
     *
     * After parsing a primary, loops on '.' to build field accesses or
     * method calls with an Optional receiver.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        Ast.Expression receiver = parsePrimaryExpression();

        while (peek(".")) {
            stream.advance(); // consume '.'

            String memberName = expectIdentifier("member name after '.'");

            if (peek("(")) {
                stream.advance(); // consume '('
                List<Ast.Expression> args = parseArgumentList();
                requireToken(")", "closing parenthesis in method call");
                receiver = new Ast.Expression.Function(Optional.of(receiver), memberName, args);
            } else {
                receiver = new Ast.Expression.Access(Optional.of(receiver), memberName);
            }
        }

        return receiver;
    }

    /**
     * Parses a primary expression — the highest-precedence / innermost rule.
     * primary ::= NIL | TRUE | FALSE | integer | decimal | character | string
     *           | '(' expression ')' | identifier ('(' args ')')?
     *
     * Handles literals, grouped expressions, plain variable access, and
     * zero-argument or multi-argument function calls.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
        // NIL / TRUE / FALSE literals
        if (peek("NIL"))   { stream.advance(); return new Ast.Expression.Literal(null); }
        if (peek("TRUE"))  { stream.advance(); return new Ast.Expression.Literal(Boolean.TRUE); }
        if (peek("FALSE")) { stream.advance(); return new Ast.Expression.Literal(Boolean.FALSE); }

        // Numeric literals
        if (peek(Token.Type.INTEGER)) {
            String raw = stream.get(0).getLiteral();
            stream.advance();
            return new Ast.Expression.Literal(new BigInteger(raw));
        }

        if (peek(Token.Type.DECIMAL)) {
            String raw = stream.get(0).getLiteral();
            stream.advance();
            return new Ast.Expression.Literal(new BigDecimal(raw));
        }

        // Character literal — strip quotes, resolve escapes, take first char
        if (peek(Token.Type.CHARACTER)) {
            String quoted = stream.get(0).getLiteral();
            stream.advance();
            char ch = resolveEscapes(quoted.substring(1, quoted.length() - 1)).charAt(0);
            return new Ast.Expression.Literal(ch);
        }

        // String literal — strip quotes and resolve escape sequences
        if (peek(Token.Type.STRING)) {
            String quoted = stream.get(0).getLiteral();
            stream.advance();
            String content = resolveEscapes(quoted.substring(1, quoted.length() - 1));
            return new Ast.Expression.Literal(content);
        }

        // Grouped expression: '(' expression ')'
        if (peek("(")) {
            stream.advance(); // consume '('
            Ast.Expression grouped = parseExpression();
            requireToken(")", "closing parenthesis in grouped expression");
            return new Ast.Expression.Group(grouped);
        }

        // Identifier: variable access or function call
        if (peek(Token.Type.IDENTIFIER)) {
            String ident = stream.get(0).getLiteral();
            stream.advance();

            if (peek("(")) {
                stream.advance(); // consume '('
                List<Ast.Expression> args = parseArgumentList();
                requireToken(")", "closing parenthesis in function call");
                return new Ast.Expression.Function(Optional.empty(), ident, args);
            }

            return new Ast.Expression.Access(Optional.empty(), ident);
        }

        throw new ParseException("Unexpected token; expected an expression.", currentIndex());
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    /**
     * Parses an expression statement or assignment statement.
     * expression ('=' expression)? ';'
     *
     * Parses the left-hand expression, then checks for '=' to decide between
     * Assignment and Expression statement nodes.
     */
    private Ast.Statement parseExpressionOrAssignment() throws ParseException {
        Ast.Expression lhs = parseExpression();

        if (peek("=")) {
            stream.advance(); // consume '='
            Ast.Expression rhs = parseExpression();
            requireSemicolon();
            return new Ast.Statement.Assignment(lhs, rhs);
        }

        requireSemicolon();
        return new Ast.Statement.Expression(lhs);
    }

    /**
     * If the current token is '=', consumes it and parses the following
     * expression, returning it wrapped in Optional. Otherwise returns empty.
     * Used by parseField() and parseDeclarationStatement().
     */
    private Optional<Ast.Expression> parseOptionalAssignment() throws ParseException {
        if (peek("=")) {
            stream.advance(); // consume '='
            return Optional.of(parseExpression());
        }
        return Optional.empty();
    }

    /**
     * Attempts to parse an {@code identifier '=' expression} assignment clause
     * for use inside a FOR header. Returns null if the clause is absent.
     * The nullable return matches Ast.Statement.For's constructor signature.
     */
    private Ast.Statement parseOptionalForAssignment() throws ParseException {
        boolean nextIsAssignment = peek(Token.Type.IDENTIFIER)
                && stream.has(1)
                && stream.get(1).getLiteral().equals("=");

        if (!nextIsAssignment) {
            return null;
        }

        String targetName = stream.get(0).getLiteral();
        stream.advance(); // consume identifier
        stream.advance(); // consume '='
        Ast.Expression assignedValue = parseExpression();

        Ast.Expression.Access target = new Ast.Expression.Access(Optional.empty(), targetName);
        return new Ast.Statement.Assignment(target, assignedValue);
    }

    /**
     * Parses a comma-separated list of expressions (function/method call args).
     * Stops before ')' without consuming it. Returns an empty list if ')' is
     * the next token.
     */
    private List<Ast.Expression> parseArgumentList() throws ParseException {
        List<Ast.Expression> args = new ArrayList<>();
        if (!peek(")")) {
            args.add(parseExpression());
            while (peek(",")) {
                stream.advance(); // consume ','
                args.add(parseExpression());
            }
        }
        return args;
    }

    /**
     * Parses a comma-separated list of identifiers (method definition params).
     * Stops before ')' without consuming it. Returns an empty list if ')' is
     * the next token.
     */
    private List<String> parseIdentifierList() throws ParseException {
        List<String> names = new ArrayList<>();
        if (peek(Token.Type.IDENTIFIER)) {
            names.add(stream.get(0).getLiteral());
            stream.advance();
            while (peek(",")) {
                stream.advance(); // consume ','
                if (!peek(Token.Type.IDENTIFIER)) {
                    throw new ParseException("Expected parameter name after ','.", currentIndex());
                }
                names.add(stream.get(0).getLiteral());
                stream.advance();
            }
        }
        return names;
    }

    /**
     * Collects statements by calling parseStatement() in a loop until the
     * stream is exhausted or one of the given stop-words is the current token.
     * The stop-word token is left unconsumed for the caller to handle.
     */
    private List<Ast.Statement> collectStatementsUntil(String... stopWords) throws ParseException {
        List<Ast.Statement> stmts = new ArrayList<>();
        while (stream.has(0) && !atAnyOf(stopWords)) {
            stmts.add(parseStatement());
        }
        return stmts;
    }

    /** Returns true if the current token's literal matches any of the given words. */
    private boolean atAnyOf(String... words) {
        if (!stream.has(0)) return false;
        String current = stream.get(0).getLiteral();
        for (String word : words) {
            if (word.equals(current)) return true;
        }
        return false;
    }

    /**
     * Returns true if the current token is one of the six comparison operators:
     * {@code < <= > >= == !=}
     */
    private boolean atComparisonOperator() {
        if (!stream.has(0)) return false;
        switch (stream.get(0).getLiteral()) {
            case "<": case "<=": case ">": case ">=": case "==": case "!=":
                return true;
            default:
                return false;
        }
    }

    /**
     * Requires the current token to be an IDENTIFIER, consumes it, and returns
     * its literal. Throws ParseException if the current token is not an identifier.
     *
     * @param role a human-readable description used in the error message (e.g. "field name")
     */
    private String expectIdentifier(String role) throws ParseException {
        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected " + role + ".", currentIndex());
        }
        String name = stream.get(0).getLiteral();
        stream.advance();
        return name;
    }

    /**
     * Requires the current token's literal to equal {@code expected} and
     * consumes it. Throws ParseException with {@code description} in the message
     * if the token does not match.
     */
    private void requireToken(String expected, String description) throws ParseException {
        if (!peek(expected)) {
            throw new ParseException("Expected " + description + " ('" + expected + "').", currentIndex());
        }
        stream.advance();
    }

    /** Convenience: requires and consumes a semicolon via requireToken. */
    private void requireSemicolon() throws ParseException {
        requireToken(";", "';'");
    }

    /**
     * Unconditionally advances the stream past the current token. Should only
     * be called after a successful peek() has confirmed the token is expected.
     */
    private void consume(String expected) {
        stream.advance();
    }

    /**
     * If the current token's literal equals {@code keyword}, advances the
     * stream and returns true. Otherwise returns false without advancing.
     */
    private boolean tryConsume(String keyword) {
        if (peek(keyword)) {
            stream.advance();
            return true;
        }
        return false;
    }

    /**
     * Walks a raw (already unquoted) string and replaces backslash escape
     * sequences with their corresponding characters.
     * Supported sequences: \b \n \r \t \' \" \\
     */
    private String resolveEscapes(String raw) {
        StringBuilder out = new StringBuilder(raw.length());
        int pos = 0;
        while (pos < raw.length()) {
            char ch = raw.charAt(pos);
            if (ch == '\\' && pos + 1 < raw.length()) {
                char escaped = raw.charAt(pos + 1);
                switch (escaped) {
                    case 'b':  out.append('\b'); break;
                    case 'n':  out.append('\n'); break;
                    case 'r':  out.append('\r'); break;
                    case 't':  out.append('\t'); break;
                    case '\'': out.append('\''); break;
                    case '"':  out.append('"');  break;
                    case '\\': out.append('\\'); break;
                    default:   out.append('\\'); out.append(escaped); break;
                }
                pos += 2;
            } else {
                out.append(ch);
                pos++;
            }
        }
        return out.toString();
    }

    /**
     * Returns the character-stream index of the current token for error
     * reporting. If the stream is exhausted, returns the index immediately
     * after the last token (last.index + last.literal.length).
     */
    private int currentIndex() {
        if (stream.has(0)) {
            return stream.get(0).getIndex();
        }
        Token lastToken = stream.get(-1);
        return lastToken.getIndex() + lastToken.getLiteral().length();
    }

    // -------------------------------------------------------------------------
    // peek / match
    // -------------------------------------------------------------------------

    /**
     * Returns {@code true} if the upcoming tokens match every pattern in order.
     * A pattern may be a {@link Token.Type} (checked against the token's type)
     * or a {@link String} (checked against the token's literal). Does not advance
     * the stream.
     */
    private boolean peek(Object... patterns) {
        for (int offset = 0; offset < patterns.length; offset++) {
            if (!stream.has(offset)) return false;
            Token candidate = stream.get(offset);
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
     * If {@link #peek(Object...)} succeeds, advances the stream past each
     * matched token and returns {@code true}. Otherwise returns {@code false}
     * without advancing.
     */
    private boolean match(Object... patterns) {
        if (!peek(patterns)) return false;
        for (int i = 0; i < patterns.length; i++) stream.advance();
        return true;
    }

    // -------------------------------------------------------------------------
    // TokenStream
    // -------------------------------------------------------------------------

    private static final class TokenStream {

        private final List<Token> tokenList;
        private int cursor = 0;

        private TokenStream(List<Token> tokenList) {
            this.tokenList = tokenList;
        }

        /** Returns {@code true} if {@code cursor + offset} is a valid index. */
        public boolean has(int offset) {
            int target = cursor + offset;
            return target >= 0 && target < tokenList.size();
        }

        /** Returns the token at {@code cursor + offset}. */
        public Token get(int offset) {
            return tokenList.get(cursor + offset);
        }

        /** Moves the cursor forward by one position. */
        public void advance() {
            cursor++;
        }
    }
}
