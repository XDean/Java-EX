package xdean.jex.extra.function;

@FunctionalInterface
public interface Action0 extends Runnable {
  void call();

  @Override
  default void run() {
    call();
  }
}
