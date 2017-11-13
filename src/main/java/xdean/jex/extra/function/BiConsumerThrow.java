package xdean.jex.extra.function;

import java.util.Objects;

import xdean.jex.internal.codecov.CodecovIgnore;

@FunctionalInterface
@CodecovIgnore
public interface BiConsumerThrow<K, V, T extends Exception> {
  void accept(K k, V v) throws T;

  default BiConsumerThrow<K, V, T> andThen(BiConsumerThrow<? super K, ? super V, T> after) {
    Objects.requireNonNull(after);
    return (l, r) -> {
      accept(l, r);
      after.accept(l, r);
    };
  }
}