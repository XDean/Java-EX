package xdean.jex.extra.rx2.nullable;

import static xdean.jex.util.function.Predicates.not;

import java.util.Optional;
import java.util.function.Function;
import java.util.stream.StreamSupport;

public class NullPolicies {
  /**
   * Drop null value
   *
   * @return
   */
  public static <T> NullPolicy<T, T> drop() {
    return i -> () -> StreamSupport.stream(i.spliterator(), false)
        .filter(not(null))
        .iterator();
  }

  /**
   * Wrap all value as Optional
   *
   * @return
   */
  public static <T> NullPolicy<T, Optional<T>> wrap() {
    return i -> () -> StreamSupport.stream(i.spliterator(), false)
        .map(Optional::ofNullable)
        .iterator();
  }

  /**
   * Replace null value by defaultValue
   *
   * @param defaultValue
   * @return
   */
  public static <T> NullPolicy<T, T> defaultValue(T defaultValue) {
    return i -> () -> StreamSupport.stream(i.spliterator(), false)
        .map(t -> t == null ? defaultValue : t)
        .iterator();
  }

  /**
   * Map values by given function
   *
   * @param function
   * @return
   */
  public static <F, T> NullPolicy<F, T> map(Function<F, T> function) {
    return i -> () -> StreamSupport.stream(i.spliterator(), false)
        .map(function)
        .iterator();
  }
}