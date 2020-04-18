package cn.xdean.jex.extra.function;

@FunctionalInterface
public interface Action3<A, B, C> {
  void call(A a, B b, C c);
}
