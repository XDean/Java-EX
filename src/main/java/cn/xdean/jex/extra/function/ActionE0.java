package cn.xdean.jex.extra.function;

@FunctionalInterface
public interface ActionE0<E extends Exception> {
  void call() throws E;
}
