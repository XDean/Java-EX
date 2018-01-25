package xdean.jex.extra.function;

@FunctionalInterface
public interface FuncE1<T, R, E extends Exception> {
  R call(T t) throws E;
}