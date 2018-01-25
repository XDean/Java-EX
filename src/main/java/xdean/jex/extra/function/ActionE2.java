package xdean.jex.extra.function;

@FunctionalInterface
public interface ActionE2<A, B, E extends Exception> {
  void call(A a, B b) throws E;
}
