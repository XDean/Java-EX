package cn.xdean.jex.lang.function.type;

@FunctionalInterface
public interface FuncE1<T, R, E extends Exception> {
  R call(T t) throws E;
}