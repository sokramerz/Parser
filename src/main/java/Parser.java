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
//throw new UnsupportedOperationException(); //TODO
        List<Ast.Field> fields = new ArrayList<>();
        List<Ast.Method> methods = new ArrayList<>();
        while (peek("LET")){
            fields.add(parseField());
        }
        while (peek("DEF")){
            methods.add(parseMethod());
        }
        if (tokens.has(0)){
            throw new ParseException("Unexpected token", tokens.get(0).getIndex());
        }
        return new Ast.Source(fields, methods);
    }
    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        match("LET");
        boolean isConst = match("CONST");
        if (!peek(Token.Type.IDENTIFIER)){
            throw new ParseException("Expected Identifier after LET (optional CONST)", getErrorIndex());
        }
        String tokenLiteral = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);
        Optional<Ast.Expression> value = Optional.empty();
        if (match("=")){
            value = Optional.of(parseExpression());
        }
        if (!match(";")){
            throw new ParseException("Missing ; at the end", getErrorIndex());
        }
        return new Ast.Field(tokenLiteral,isConst,value);
    }
    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        match("DEF");
        if (!peek(Token.Type.IDENTIFIER)){
            throw new ParseException("Expected Identifier after DEF", getErrorIndex());
        }
        String tokenLiteral = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);
        if (!match("(")){
            throw new ParseException("Missing ( at start of method def", getErrorIndex());
        }
        List<String> parameters = new ArrayList<>();
        if (peek(Token.Type.IDENTIFIER)){
            parameters.add(tokens.get(0).getLiteral());
            match(Token.Type.IDENTIFIER);
            while (match(",")){
                if (!peek(Token.Type.IDENTIFIER)){
                    throw new ParseException("Missing identifier after , in method statement", getErrorIndex());
                }
                parameters.add(tokens.get(0).getLiteral());
                match(Token.Type.IDENTIFIER);
            }
        }
        if (!match(")")){
            throw new ParseException("Missing ) after list of parameters", getErrorIndex());
        }
        if (!match("DO")){
            throw new ParseException("Missing DO", getErrorIndex());
        }
        List<Ast.Statement> statements = new ArrayList<>();
        while (!peek("END")){
            if (!tokens.has(0)){
                throw new ParseException("Expected END", getErrorIndex());
            }
            statements.add(parseStatement());
        }
        if (!match("END")){
            throw new ParseException("Missing END at end", getErrorIndex());
        }
        return new Ast.Method(tokenLiteral,parameters,statements);
    }
    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        if (peek("LET")) {
            return parseDeclarationStatement();
        }
        else if (peek("IF")) {
            return parseIfStatement();
        }
        else if (peek("FOR")) {
            return parseForStatement();
        }
        else if (peek("WHILE")) {
            return parseWhileStatement();
        }
        else if (peek("RETURN")) {
            return parseReturnStatement();
        }
        else {
            Ast.Expression leftExpression = parseExpression();
            if (match("=")) {
                Ast.Expression expressionVal = parseExpression();
                if (!match(";")) {
                    throw new ParseException("Missing \";\"", getErrorIndex());
                }
                return new Ast.Statement.Assignment(leftExpression, expressionVal);
            }
            else {
                if (!match(";")) {
                    throw new ParseException("Missing \";\"", getErrorIndex());
                }
                return new Ast.Statement.Expression(leftExpression);
            }
        }
    }
    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
// throw new UnsupportedOperationException(); //TODO
        match("LET");
        if (!peek(Token.Type.IDENTIFIER)){
            throw new ParseException("Expected Identifier after LET", getErrorIndex());
        }
        String tokenLiteral = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);
        Optional<Ast.Expression> value = Optional.empty();
        if (match("=")) {
            value = Optional.of(parseExpression());
        }
        if (!match(";")) {
            throw new ParseException("Missing \";\" at end of statement", getErrorIndex());
        }
        return new Ast.Statement.Declaration(tokenLiteral,value);
    }
    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
// throw new UnsupportedOperationException(); //TODO
        match("IF");
        Ast.Expression condition = parseExpression();
        if (!match("DO")){
            throw new ParseException("Missing DO after expression in IF statement", getErrorIndex());
        }
        List<Ast.Statement> thenStatements = new ArrayList<>();
        while ((!peek("ELSE")) && (!peek("END"))){
            if (!tokens.has(0)){
                throw new ParseException("Missing END at if statement", getErrorIndex());
            }
            thenStatements.add(parseStatement());
        }
        List<Ast.Statement> elseStatements = new ArrayList<>();
        if (match("ELSE")) {
            while (!peek("END")){
                if (!tokens.has(0)){
                    throw new ParseException("Missing END at if statement", getErrorIndex());
                }
                elseStatements.add(parseStatement());
            }
        }
        if (!match("END")){
            throw new ParseException("Missing END at end", getErrorIndex());
        }
        return new Ast.Statement.If(condition,thenStatements,elseStatements);
    }
    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        match("FOR");
        if (!match("(")){
            throw new ParseException("Missing ( after FOR", getErrorIndex());
        }
        Ast.Statement initialization = null;
        if (peek(Token.Type.IDENTIFIER)){
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if (!match("=")){
                throw new ParseException("Expected = in FOR initialization", getErrorIndex());
            }
            Ast.Expression expr = parseExpression();
            initialization = new Ast.Statement.Assignment(
                    new Ast.Expression.Access(Optional.empty(), name), expr);
        }
        if (!match(";")){
            throw new ParseException("Missing ; in FOR statement", getErrorIndex());
        }
        Ast.Expression condition = parseExpression();
        if (!match(";")){
            throw new ParseException("Missing ; after condition in FOR statement", getErrorIndex());
        }
        Ast.Statement increment = null;
        if (peek(Token.Type.IDENTIFIER)){
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if (!match("=")){
                throw new ParseException("Expected = in FOR increment", getErrorIndex());
            }
            Ast.Expression expr = parseExpression();
            increment = new Ast.Statement.Assignment(
                    new Ast.Expression.Access(Optional.empty(), name), expr);
        }
        if (!match(")")){
            throw new ParseException("Missing ) after FOR header", getErrorIndex());
        }
        List<Ast.Statement> statements = new ArrayList<>();
        while (!peek("END")){
            if (!tokens.has(0)){
                throw new ParseException("Missing END in FOR statement", getErrorIndex());
            }
            statements.add(parseStatement());
        }
        if (!match("END")){
            throw new ParseException("Missing END at end of FOR", getErrorIndex());
        }
        return new Ast.Statement.For(initialization, condition, increment, statements);
    }
    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        match("WHILE");
        Ast.Expression condition = parseExpression();
        if (!match("DO")){
            throw new ParseException("Missing DO after WHILE condition", getErrorIndex());
        }
        List<Ast.Statement> statements = new ArrayList<>();
        while (!peek("END")){
            if (!tokens.has(0)){
                throw new ParseException("Missing END in WHILE statement", getErrorIndex());
            }
            statements.add(parseStatement());
        }
        if (!match("END")){
            throw new ParseException("Missing END at end of WHILE", getErrorIndex());
        }
        return new Ast.Statement.While(condition, statements);
    }
    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
// throw new UnsupportedOperationException(); //TODO
        match("RETURN");
        Ast.Expression val = parseExpression();
        if (!match(";")) {
            throw new ParseException("Missing \";\" after RETURN", getErrorIndex());
        }
        return new Ast.Statement.Return(val);
    }
    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        return parseLogicalExpression();
    }
    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        Ast.Expression leftExpr = parseEqualityExpression();
        while (peek("&&") || peek("||") || peek("AND") || peek("OR")){
            String tokenOperator = tokens.get(0).getLiteral();
            match(tokenOperator);
            Ast.Expression rightExpr = parseEqualityExpression();
            leftExpr = new Ast.Expression.Binary(tokenOperator, leftExpr,rightExpr);
        }
        return leftExpr;
    }
    /**
     * Parses the {@code equality-expression} rule. - comparison_expression
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        Ast.Expression leftExpr = parseAdditiveExpression();
        while (peek("<") || peek("<=") || peek(">") || peek (">=") || peek("==") || peek ("!=")){
            String tokenOperator = tokens.get(0).getLiteral();
            match(tokenOperator);
            Ast.Expression rightExpr = parseAdditiveExpression();
            leftExpr = new Ast.Expression.Binary(tokenOperator, leftExpr, rightExpr);
        }
        return leftExpr;
    }
    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        Ast.Expression leftExpr = parseMultiplicativeExpression();
        while(peek("+") || peek("-")){
            String tokenOperator = tokens.get(0).getLiteral();
            match(tokenOperator);
            Ast.Expression rightExpr = parseMultiplicativeExpression();
            leftExpr = new Ast.Expression.Binary(tokenOperator, leftExpr, rightExpr);
        }
        return leftExpr;
    }
    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        Ast.Expression leftExpr = parseSecondaryExpression();
        while(peek("*") || peek("/")){
            String tokenOperator = tokens.get(0).getLiteral();
            match(tokenOperator);
            Ast.Expression rightExpr = parseSecondaryExpression();
            leftExpr = new Ast.Expression.Binary(tokenOperator, leftExpr, rightExpr);
        }
        return leftExpr;
    }
    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        Ast.Expression leftExpr = parsePrimaryExpression();
        while(match(".")) {
            if (!peek(Token.Type.IDENTIFIER)) {
                throw new ParseException("Missing identifier after \".\"", getErrorIndex());
            }
            String tokenLiteral = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if (match("(")) {
                List<Ast.Expression> args = new ArrayList<>();
                if (!peek(")")) {
                    args.add(parseExpression());
                    while (match(",")) {
                        args.add(parseExpression());
                    }
                }
                if (!match(")")) {
                    throw new ParseException("Missing \")\" after", getErrorIndex());
                }
                leftExpr = new Ast.Expression.Function(Optional.of(leftExpr), tokenLiteral, args);
            }
            else {
                leftExpr = new Ast.Expression.Access(Optional.of(leftExpr), tokenLiteral);
            }
        }
        return leftExpr;
    }
    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
//throw new UnsupportedOperationException(); //TODO
        if (match("NIL")){
            return new Ast.Expression.Literal(null);
        }
        else if (match("TRUE")){
            return new Ast.Expression.Literal(true);
        }
        else if (match("FALSE")){
            return new Ast.Expression.Literal(false);
        }
        else if (peek(Token.Type.INTEGER)){
            Token currentMatchedToken = tokens.get(0);
            tokens.advance(); // moves to the next token, already peeked current token
            String literalText = currentMatchedToken.getLiteral();
            BigInteger value = new BigInteger(literalText);
            return new Ast.Expression.Literal(value);
        }
        else if (peek(Token.Type.DECIMAL)){
            Token currentMatchedToken = tokens.get(0);
            tokens.advance(); // moves to the next token, already peeked current token
            String literalText = currentMatchedToken.getLiteral();
            BigDecimal value = new BigDecimal(literalText);
            return new Ast.Expression.Literal(value);
        }
        else if (peek(Token.Type.CHARACTER)){
            String tokenLiteral = tokens.get(0).getLiteral();
            tokens.advance(); // moves to the next token, already peeked current token
            String tokenLiteralValue = tokenLiteral.substring(1, tokenLiteral.length() - 1); // removes ''
            char tokenLiteralValueChar = tokenLiteralValue.charAt(0);
            return new Ast.Expression.Literal(tokenLiteralValueChar);
        }
        else if (peek(Token.Type.STRING)){
            String tokenLiteral = tokens.get(0).getLiteral();
            tokens.advance(); // moves to the next token, already peeked current token
            String tokenLiteralValue = tokenLiteral.substring(1, tokenLiteral.length() - 1); // removes ""
            String updatedString = "";
            for (int i = 0; i < tokenLiteralValue.length() ; i++){
                char c = tokenLiteralValue.charAt(i);
                if (c == '\\'){
                    if (i+1 >= tokenLiteralValue.length()){
                        throw new ParseException("Invalid escape", getErrorIndex());
                    }
                    char esc = tokenLiteralValue.charAt(++i);
                    if (esc == 'b') {
                        updatedString += '\b';
                    }
                    else if (esc == 'n') {
                        updatedString += '\n';
                    }
                    else if (esc == 'r') {
                        updatedString += '\r';
                    }
                    else if (esc == 't') {
                        updatedString += '\t';
                    }
                    else if (esc == '\'') {
                        updatedString += '\'';
                    }
                    else if (esc == '"') {
                        updatedString += '"';
                    }
                    else if (esc == '\\') {
                        updatedString += "\\";
                    }
                    else {
                        throw new ParseException("Invalid escape", getErrorIndex());
                    }
                }
                else {
                    updatedString += c;
                }
            }
            return new Ast.Expression.Literal(updatedString);
        }
        else if (match("(")) {
            Ast.Expression expr = parseExpression();
            if (!peek(")")) {
                throw new ParseException("Missing \")\" after expression", getErrorIndex());
            }
            match(")");
            return new Ast.Expression.Group(expr);
        }
        else if (peek(Token.Type.IDENTIFIER)) {
            String tokenLiteral = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if (match("(")) {
                List<Ast.Expression> args = new ArrayList<>();
                if (!peek(")")) {
                    args.add(parseExpression());
                    while (match(",")) {
                        args.add(parseExpression());
                    }
                }
                if (!match(")")) {
                    throw new ParseException("Missing \")\"", getErrorIndex());
                }
                return new Ast.Expression.Function(Optional.empty(), tokenLiteral, args);
            } else {
                return new Ast.Expression.Access(Optional.empty(), tokenLiteral);
            }
        }
        else {
            throw new ParseException("Invalid Primary Expression", getErrorIndex());
        }
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
//throw new UnsupportedOperationException(); //TODO (in lecture)
        for (int i = 0; i < patterns.length; i++) {
// check for token
            if (!tokens.has(i)) {
                return false;
            }
// check if pattern is type token
// if token type does not match pattern -> false
            if (patterns[i] instanceof Token.Type){
                if (tokens.get(i).getType() != patterns[i]){
                    return false;
                }
            }
// check if pattern is a string
// if token's literal does not match pattern -> false
            else if (patterns[i] instanceof String){
                if (!(tokens.get(i).getLiteral().equals(patterns[i]))){
                    return false;
                }
            }
        }
        return true;
    }
    /**
     * As in the lexer, returns {@code true} if {@link #peek(Object...)} is true
     * and advances the token stream.
     */
    private boolean match(Object... patterns) {
//throw new UnsupportedOperationException(); //TODO (in lecture)
        boolean peek = peek(patterns);
        if (peek) {
            for (int i = 0; i < patterns.length; i++) {
                tokens.advance();
            }
        }
        return peek;
    }
    private int getErrorIndex() {
        if (tokens.has(0)){
            return tokens.get(0).getIndex();
        }
        else {
            Token lastToken = tokens.get(-1);
            return lastToken.getIndex() + lastToken.getLiteral().length();
        }
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
            return index + offset < tokens.size() && index + offset >= 0;
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
