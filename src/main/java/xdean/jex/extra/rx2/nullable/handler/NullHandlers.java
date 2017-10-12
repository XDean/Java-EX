package xdean.jex.extra.rx2.nullable.handler;

import java.util.Optional;

public interface NullHandlers {

  /**
   * Drop null value
   *
   * @return
   */
  static <T> NullHandler<T, T> drop() {
    return i -> i;
  }

  /**
   * Wrap all value as Optional
   *
   * @return
   */
  static <T> NullHandler<T, Optional<T>> wrap() {
    return Optional::ofNullable;
  }

  /**
   * Replace null value by defaultValue
   *
   * @param defaultValue
   * @return
   */
  static <T> NullHandler<T, T> defaultValue(T defaultValue) {
    return i -> i == null ? defaultValue : i;
  }

  /**
   * Do something on null value
   *
   * @param action
   * @return
   */
  static <T> NullHandler<T, T> run(Runnable action) {
    return i -> {
      if (i == null) {
        action.run();
      }
      return i;
    };
  }
}