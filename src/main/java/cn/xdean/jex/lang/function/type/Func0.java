package cn.xdean.jex.lang.function.type;

import java.util.function.Supplier;

@FunctionalInterface
public interface Func0<R> extends Supplier<R> {
  R call();

  @Override
  default R get() {
    return call();
  }
}
