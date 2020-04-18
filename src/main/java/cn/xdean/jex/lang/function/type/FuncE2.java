package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface FuncE2<A, B, R, E extends Exception> {
  R call(A a, B b) throws E;
}