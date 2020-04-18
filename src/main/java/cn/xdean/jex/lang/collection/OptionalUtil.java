package cn.xdean.jex.lang.collection;

import cn.xdean.jex.lang.function.type.ActionE0;
import cn.xdean.jex.lang.function.type.ActionE1;

import java.util.Optional;

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
