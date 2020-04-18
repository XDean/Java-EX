package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface ActionE0<E extends Exception> {
  void call() throws E;
}
