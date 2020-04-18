package cn.xdean.jex.util;

import java.util.Optional;

import cn.xdean.jex.extra.function.ActionE0;
import cn.xdean.jex.extra.function.ActionE1;

public class OptionalUtil {
  public static <T, E extends Exception> Optional<T> ifEmpty(Optional<T> o, ActionE0<E> r) throws E {
    if (o.isPresent() == false) {
      r.call();
    }
    return o;
  }

  public static <T, E extends Exception> Optional<T> ifPresent(Optional<T> o, ActionE1<T, E> r) throws E {
    if (o.isPresent()) {
      r.call(o.get());
    }
    return o;
  }
}
