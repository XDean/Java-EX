package cn.xdean.jex.lang.function.type;

import xdean.codecov.CodecovIgnore;

import java.util.function.BiConsumer;

@CodecovIgnore
@FunctionalInterface
public interface Action2<A, B> extends BiConsumer<A, B> {
  void call(A a, B b);

  @Override
  default void accept(A a, B b) {
    call(a, b);
  }
}
