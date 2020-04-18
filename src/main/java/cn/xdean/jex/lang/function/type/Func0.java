package cn.xdean.jex.lang.function.type;

import xdean.codecov.CodecovIgnore;

import java.util.function.Supplier;

@CodecovIgnore
@FunctionalInterface
public interface Func0<R> extends Supplier<R> {
  R call();

  @Override
  default R get() {
    return call();
  }
}
