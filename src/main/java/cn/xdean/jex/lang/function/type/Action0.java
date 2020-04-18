package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface Action0 extends Runnable {
  void call();

  @Override
  default void run() {
    call();
  }
}
