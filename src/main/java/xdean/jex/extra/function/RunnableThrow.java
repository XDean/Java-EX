package xdean.jex.extra.function;

@FunctionalInterface
public interface RunnableThrow<T extends Throwable> {
  void run() throws T;
}