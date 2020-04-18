package cn.xdean.jex.extra.function;

@FunctionalInterface
public interface ActionE1<A, E extends Exception> {
  void call(A a) throws E;
}
