package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface Action3<A, B, C> {
  void call(A a, B b, C c);
}
