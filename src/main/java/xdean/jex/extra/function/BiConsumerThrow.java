package xdean.jex.extra.function;

import java.util.Objects;

@FunctionalInterface
public interface BiConsumerThrow<K, V, T extends Throwable> {
  void accept(K k, V v) throws T;

  default BiConsumerThrow<K, V, T> andThen(BiConsumerThrow<? super K, ? super V, T> after) {
    Objects.requireNonNull(after);
    return (l, r) -> {
      accept(l, r);
      after.accept(l, r);
    };
  }
}