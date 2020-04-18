package cn.xdean.jex.extra.function;

@FunctionalInterface
public interface FuncE2<A, B, R, E extends Exception> {
  R call(A a, B b) throws E;
}