package xdean.jex.util;

import java.util.Optional;

import xdean.jex.extra.function.ConsumerThrow;
import xdean.jex.extra.function.RunnableThrow;
import lombok.experimental.UtilityClass;

@UtilityClass
public class OptionalUtil {

  public <T> Optional<T> ifEmpty(Optional<T> o, Runnable r) {
    if (o.isPresent() == false) {
      r.run();
    }
    return o;
  }

  public <T, E extends Throwable> Optional<T> ifEmpty(Optional<T> o, RunnableThrow<E> r) throws E {
    if (o.isPresent() == false) {
      r.run();
    }
    return o;
  }

  public <T, E extends Throwable> Optional<T> ifPresent(Optional<T> o, ConsumerThrow<T, E> r) throws E {
    if (o.isPresent()) {
      r.accept(o.get());
    }
    return o;
  }
}
