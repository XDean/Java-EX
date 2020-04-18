package cn.xdean.jex.lang.function.type;

import java.util.function.Function;

@FunctionalInterface
public interface Func1<A, R> extends Function<A, R> {
  R call(A a);

  @Override
  default R apply(A a) {
    return call(a);
  }
}
