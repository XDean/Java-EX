package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface ActionE3<A, B, C, E extends Exception> {
  void call(A a, B b, C c) throws E;
}
