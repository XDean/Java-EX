package cn.xdean.jex.lang.function.type;

import xdean.codecov.CodecovIgnore;

import java.util.function.Consumer;

@CodecovIgnore
@FunctionalInterface
public interface Action1<A> extends Consumer<A> {
  void call(A a);

  @Override
  default void accept(A a) {
    call(a);
  }
}
