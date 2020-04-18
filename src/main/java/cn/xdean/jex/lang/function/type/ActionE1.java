package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface ActionE1<A, E extends Exception> {
  void call(A a) throws E;
}
