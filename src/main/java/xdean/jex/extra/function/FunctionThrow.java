package xdean.jex.extra.function;

import java.util.Objects;

import xdean.jex.internal.codecov.CodecovIgnore;

@FunctionalInterface
@CodecovIgnore
public interface FunctionThrow<F, R, T extends Exception> {

  R apply(F f) throws T;

  default <V> FunctionThrow<V, R, T> compose(FunctionThrow<? super V, ? extends F, T> before) {
    Objects.requireNonNull(before);
    return (V v) -> apply(before.apply(v));
  }

  default <V> FunctionThrow<F, V, T> andThen(FunctionThrow<? super R, ? extends V, T> after) {
    Objects.requireNonNull(after);
    return (F t) -> after.apply(apply(t));
  }

  static <F, T extends Exception> FunctionThrow<F, F, T> identity() {
    return t -> t;
  }
}