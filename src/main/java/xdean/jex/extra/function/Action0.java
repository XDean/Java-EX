package xdean.jex.extra.function;

import xdean.codecov.CodecovIgnore;

@CodecovIgnore
@FunctionalInterface
public interface Action0 extends Runnable {
  void call();

  @Override
  default void run() {
    call();
  }
}
