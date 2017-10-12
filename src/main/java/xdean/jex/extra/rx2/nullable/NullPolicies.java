package xdean.jex.extra.rx2.nullable;

import java.util.Optional;

public interface NullPolicies {

  /**
   * Drop null value
   *
   * @return
   */
  static <T> NullPolicy<T, T> drop() {
    return i -> i;
  }

  /**
   * Wrap all value as Optional
   *
   * @return
   */
  static <T> NullPolicy<T, Optional<T>> wrap() {
    return Optional::ofNullable;
  }

  /**
   * Replace null value by defaultValue
   *
   * @param defaultValue
   * @return
   */
  static <T> NullPolicy<T, T> defaultValue(T defaultValue) {
    return i -> i == null ? defaultValue : i;
  }

  /**
   * Do something on null value
   *
   * @param action
   * @return
   */
  static <T> NullPolicy<T, T> run(Runnable action) {
    return i -> {
      action.run();
      return i;
    };
  }
}