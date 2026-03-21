package plc.project;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Interpreter implements Ast.Visitor<Environment.PlcObject> {

    private Scope scope = new Scope(null);

    public Interpreter(Scope parent) {
        scope = new Scope(parent);
        scope.defineFunction("print", 1, args -> {
            System.out.println(args.get(0).getValue());
            return Environment.NIL;
        });
    }

    public Scope getScope() {
        return scope;
    }

    @Override
    public Environment.PlcObject visit(Ast.Source ast) {
        for (Ast.Field f : ast.getFields()) {
            visit(f);
        }
        for (Ast.Method m : ast.getMethods()) {
            visit(m);
        }
        return scope.lookupFunction("main", 0).invoke(new ArrayList<>());
    }

    @Override
    public Environment.PlcObject visit(Ast.Field ast) {
        Environment.PlcObject val = Environment.NIL;
        if (ast.getValue().isPresent()) {
            val = visit(ast.getValue().get());
        }
        scope.defineVariable(ast.getName(), ast.getConstant(), val);
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Method ast) {
        // need to grab scope here or the closure won't see the right variables
        Scope defScope = scope;

        scope.defineFunction(ast.getName(), ast.getParameters().size(), args -> {
            Scope prev = scope;
            scope = new Scope(defScope);
            try {
                for (int i = 0; i < ast.getParameters().size(); i++) {
                    scope.defineVariable(ast.getParameters().get(i), false, args.get(i));
                }
                for (Ast.Statement stmt : ast.getStatements()) {
                    visit(stmt);
                }
                return Environment.NIL;
            } catch (Return r) {
                return r.value;
            } finally {
                scope = prev;
            }
        });
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Expression ast) {
        visit(ast.getExpression());
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Declaration ast) {
        Environment.PlcObject val = Environment.NIL;
        if (ast.getValue().isPresent()) {
            val = visit(ast.getValue().get());
        }
        scope.defineVariable(ast.getName(), false, val);
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Assignment ast) {
        if (!(ast.getReceiver() instanceof Ast.Expression.Access)) {
            throw new RuntimeException("Invalid assignment target.");
        }
        Ast.Expression.Access access = (Ast.Expression.Access) ast.getReceiver();
        Environment.PlcObject val = visit(ast.getValue());

        if (access.getReceiver().isPresent()) {
            Environment.PlcObject obj = visit(access.getReceiver().get());
            obj.setField(access.getName(), val);
        } else {
            scope.lookupVariable(access.getName()).setValue(val);
        }
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.If ast) {
        boolean cond = requireType(Boolean.class, visit(ast.getCondition()));
        Scope prev = scope;
        scope = new Scope(prev);
        try {
            if (cond) {
                for (Ast.Statement stmt : ast.getThenStatements()) {
                    visit(stmt);
                }
            } else {
                for (Ast.Statement stmt : ast.getElseStatements()) {
                    visit(stmt);
                }
            }
        } finally {
            scope = prev;
        }
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.For ast) {
        if (ast.getInitialization() != null) {
            visit(ast.getInitialization());
        }
        while (requireType(Boolean.class, visit(ast.getCondition()))) {
            Scope prev = scope;
            scope = new Scope(prev);
            try {
                for (Ast.Statement stmt : ast.getStatements()) {
                    visit(stmt);
                }
            } finally {
                scope = prev;
            }
            if (ast.getIncrement() != null) {
                visit(ast.getIncrement());
            }
        }
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.While ast) {
        while (requireType(Boolean.class, visit(ast.getCondition()))) {
            Scope prev = scope;
            scope = new Scope(prev);
            try {
                for (Ast.Statement stmt : ast.getStatements()) {
                    visit(stmt);
                }
            } finally {
                scope = prev;
            }
        }
        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Return ast) {
        throw new Return(visit(ast.getValue()));
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Literal ast) {
        if (ast.getLiteral() == null) {
            return Environment.NIL;
        }
        return Environment.create(ast.getLiteral());
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Group ast) {
        return visit(ast.getExpression());
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Binary ast) {
        String op = ast.getOperator();

        if (op.equals("AND")) {
            boolean lhs = requireType(Boolean.class, visit(ast.getLeft()));
            if (!lhs) return Environment.create(false);
            boolean rhs = requireType(Boolean.class, visit(ast.getRight()));
            return Environment.create(rhs);
        } else if (op.equals("OR")) {
            boolean lhs = requireType(Boolean.class, visit(ast.getLeft()));
            if (lhs) return Environment.create(true);
            boolean rhs = requireType(Boolean.class, visit(ast.getRight()));
            return Environment.create(rhs);
        } else if (op.equals("==")) {
            Object lhs = visit(ast.getLeft()).getValue();
            Object rhs = visit(ast.getRight()).getValue();
            return Environment.create(Objects.equals(lhs, rhs));
        } else if (op.equals("!=")) {
            Object lhs = visit(ast.getLeft()).getValue();
            Object rhs = visit(ast.getRight()).getValue();
            return Environment.create(!Objects.equals(lhs, rhs));
        } else if (op.equals("<") || op.equals("<=") || op.equals(">") || op.equals(">=")) {
            Comparable lhs = requireType(Comparable.class, visit(ast.getLeft()));
            Object rhs = visit(ast.getRight()).getValue();
            if (!lhs.getClass().equals(rhs.getClass())) {
                throw new RuntimeException("Type mismatch for operator: " + op);
            }
            int cmp = lhs.compareTo(rhs);
            if (op.equals("<"))       return Environment.create(cmp < 0);
            else if (op.equals("<=")) return Environment.create(cmp <= 0);
            else if (op.equals(">"))  return Environment.create(cmp > 0);
            else                      return Environment.create(cmp >= 0);
        } else if (op.equals("+")) {
            Environment.PlcObject lhsObj = visit(ast.getLeft());
            Environment.PlcObject rhsObj = visit(ast.getRight());
            Object lhs = lhsObj.getValue();
            Object rhs = rhsObj.getValue();
            if (lhs instanceof String || rhs instanceof String) {
                return Environment.create(lhs.toString() + rhs.toString());
            } else if (lhs instanceof BigInteger) {
                return Environment.create(((BigInteger) lhs).add(requireType(BigInteger.class, rhsObj)));
            } else if (lhs instanceof BigDecimal) {
                return Environment.create(((BigDecimal) lhs).add(requireType(BigDecimal.class, rhsObj)));
            }
            throw new RuntimeException("Unsupported type for +");
        } else if (op.equals("-")) {
            Environment.PlcObject lhsObj = visit(ast.getLeft());
            Environment.PlcObject rhsObj = visit(ast.getRight());
            Object lhs = lhsObj.getValue();
            if (lhs instanceof BigInteger) {
                return Environment.create(((BigInteger) lhs).subtract(requireType(BigInteger.class, rhsObj)));
            } else if (lhs instanceof BigDecimal) {
                return Environment.create(((BigDecimal) lhs).subtract(requireType(BigDecimal.class, rhsObj)));
            }
            throw new RuntimeException("Unsupported type for -");
        } else if (op.equals("*")) {
            Environment.PlcObject lhsObj = visit(ast.getLeft());
            Environment.PlcObject rhsObj = visit(ast.getRight());
            Object lhs = lhsObj.getValue();
            if (lhs instanceof BigInteger) {
                return Environment.create(((BigInteger) lhs).multiply(requireType(BigInteger.class, rhsObj)));
            } else if (lhs instanceof BigDecimal) {
                return Environment.create(((BigDecimal) lhs).multiply(requireType(BigDecimal.class, rhsObj)));
            }
            throw new RuntimeException("Unsupported type for *");
        } else if (op.equals("/")) {
            Environment.PlcObject lhsObj = visit(ast.getLeft());
            Environment.PlcObject rhsObj = visit(ast.getRight());
            Object lhs = lhsObj.getValue();
            if (lhs instanceof BigInteger) {
                BigInteger rhs = requireType(BigInteger.class, rhsObj);
                if (rhs.equals(BigInteger.ZERO)) {
                    throw new RuntimeException("Divide by zero.");
                }
                return Environment.create(((BigInteger) lhs).divide(rhs));
            } else if (lhs instanceof BigDecimal) {
                BigDecimal rhs = requireType(BigDecimal.class, rhsObj);
                if (rhs.compareTo(BigDecimal.ZERO) == 0) {
                    throw new RuntimeException("Divide by zero.");
                }
                return Environment.create(((BigDecimal) lhs).divide(rhs, RoundingMode.HALF_EVEN));
            }
            throw new RuntimeException("Unsupported type for /");
        }

        throw new RuntimeException("Unknown operator: " + op);
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Access ast) {
        if (ast.getReceiver().isPresent()) {
            Environment.PlcObject obj = visit(ast.getReceiver().get());
            return obj.getField(ast.getName()).getValue();
        }
        return scope.lookupVariable(ast.getName()).getValue();
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Function ast) {
        List<Environment.PlcObject> args = new ArrayList<>();
        for (Ast.Expression arg : ast.getArguments()) {
            args.add(visit(arg));
        }
        if (ast.getReceiver().isPresent()) {
            Environment.PlcObject obj = visit(ast.getReceiver().get());
            return obj.callMethod(ast.getName(), args);
        }
        return scope.lookupFunction(ast.getName(), args.size()).invoke(args);
    }

    /**
     * Helper function to ensure an object is of the appropriate type.
     */
    private static <T> T requireType(Class<T> type, Environment.PlcObject object) {
        if (type.isInstance(object.getValue())) {
            return type.cast(object.getValue());
        } else {
            throw new RuntimeException("Expected type " + type.getName() + ", received " + object.getValue().getClass().getName() + ".");
        }
    }

    /**
     * Exception class for returning values.
     */
    private static class Return extends RuntimeException {

        private final Environment.PlcObject value;

        private Return(Environment.PlcObject value) {
            this.value = value;
        }

    }

}
