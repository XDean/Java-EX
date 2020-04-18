package cn.xdean.jex.extra.function;

import java.util.function.BiConsumer;

import xdean.codecov.CodecovIgnore;

@CodecovIgnore
@FunctionalInterface
public interface Action2<A, B> extends BiConsumer<A, B> {
  void call(A a, B b);

  @Override
  default void accept(A a, B b) {
    call(a, b);
  }
}
