package cn.xdean.jex.extra.function;

@FunctionalInterface
public interface FuncE3<A, B, C, R, E extends Exception> {
  R call(A a, B b, C c) throws E;
}