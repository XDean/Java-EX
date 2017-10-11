package xdean.jex.extra.rx2.nullable;

import java.util.Optional;

public class NullPolicies {
  /**
   * Drop null value
   *
   * @return
   */
  public static <T> NullPolicy<T, T> drop() {
    return i -> i;
  }

  /**
   * Wrap all value as Optional
   *
   * @return
   */
  public static <T> NullPolicy<T, Optional<T>> wrap() {
    return Optional::ofNullable;
  }

  /**
   * Replace null value by defaultValue
   *
   * @param defaultValue
   * @return
   */
  public static <T> NullPolicy<T, T> defaultValue(T defaultValue) {
    return i -> i == null ? defaultValue : i;
  }
}