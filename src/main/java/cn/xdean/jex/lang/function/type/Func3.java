package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface Func3<A, B, C, R> {
  R call(A a, B b, C c);
}
