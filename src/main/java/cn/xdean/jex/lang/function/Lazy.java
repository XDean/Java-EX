package cn.xdean.jex.lang.function;

import java.util.function.Supplier;

public class Lazy<T> {

  public static <T> Lazy<T> create(Supplier<T> supplier) {
    return new Lazy<>(supplier);
  }

  private Supplier<T> creater;
  private boolean calculated = false;
  private T value = null;

  private Lazy(Supplier<T> supplier) {
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
