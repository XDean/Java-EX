package xdean.jex.extra.function;

@FunctionalInterface
public interface RunnableThrow<T extends Exception> {
  void run() throws T;
}