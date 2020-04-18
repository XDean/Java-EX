package cn.xdean.jex.lang.function.type;

import xdean.codecov.CodecovIgnore;

import java.util.function.BiFunction;

@CodecovIgnore
@FunctionalInterface
public interface Func2<A, B, R> extends BiFunction<A, B, R> {
  R call(A a, B b);

  @Override
  default R apply(A a, B b) {
    return call(a, b);
  }
}
