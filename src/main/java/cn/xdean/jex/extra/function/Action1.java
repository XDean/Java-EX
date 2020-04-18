package cn.xdean.jex.extra.function;

import java.util.function.Consumer;

import xdean.codecov.CodecovIgnore;

@CodecovIgnore
@FunctionalInterface
public interface Action1<A> extends Consumer<A> {
  void call(A a);

  @Override
  default void accept(A a) {
    call(a);
  }
}
