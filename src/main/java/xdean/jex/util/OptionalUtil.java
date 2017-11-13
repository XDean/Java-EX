package xdean.jex.util;

import java.util.Optional;

import xdean.jex.extra.function.ConsumerThrow;
import xdean.jex.extra.function.RunnableThrow;

public class OptionalUtil {
  public static <T, E extends Exception> Optional<T> ifEmpty(Optional<T> o, RunnableThrow<E> r) throws E {
    if (o.isPresent() == false) {
      r.run();
    }
    return o;
  }

  public static <T, E extends Exception> Optional<T> ifPresent(Optional<T> o, ConsumerThrow<T, E> r) throws E {
    if (o.isPresent()) {
      r.accept(o.get());
    }
    return o;
  }
}
