package xdean.jex.extra.function;

@FunctionalInterface
public interface Func3<A, B, C, R> {
  R call(A a, B b, C c);
}
