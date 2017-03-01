package xdean.jex.extra.function;

import java.util.Objects;

@FunctionalInterface
public interface ConsumerThrow<T, E extends Throwable> {

  void accept(T t) throws E;

  default ConsumerThrow<T, E> andThen(ConsumerThrow<? super T, E> after) {
    Objects.requireNonNull(after);
    return (T t) -> {
      accept(t);
      after.accept(t);
    };
  }
}