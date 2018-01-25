package xdean.jex.extra.function;

import java.util.function.BiFunction;

@FunctionalInterface
public interface Func2<A, B, R> extends BiFunction<A, B, R> {
  R call(A a, B b);

  @Override
  default R apply(A a, B b) {
    return call(a, b);
  }
}
