package xdean.jex.extra;

import java.util.function.Supplier;

public class LazyValue<T> {

  public static <T> LazyValue<T> create(Supplier<T> supplier) {
    return new LazyValue<>(supplier);
  }

  private Supplier<T> creater;
  private boolean calculated = false;
  private T value = null;

  private LazyValue(Supplier<T> supplier) {
    creater = supplier;
  }

  public T get() {
    if (calculated == false) {
      synchronized (this) {
        if (calculated == false) {
          value = creater.get();
          creater = null;
          calculated = true;
        }
      }
    }
    return value;
  }
}
