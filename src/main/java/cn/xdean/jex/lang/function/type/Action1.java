package cn.xdean.jex.lang.function.type;

import java.util.function.Consumer;

@FunctionalInterface
public interface Action1<A> extends Consumer<A> {
  void call(A a);

  @Override
  default void accept(A a) {
    call(a);
  }
}
