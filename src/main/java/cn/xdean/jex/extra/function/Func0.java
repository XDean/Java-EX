package cn.xdean.jex.extra.function;

import java.util.function.Supplier;

import xdean.codecov.CodecovIgnore;

@CodecovIgnore
@FunctionalInterface
public interface Func0<R> extends Supplier<R> {
  R call();

  @Override
  default R get() {
    return call();
  }
}
