package xdean.jex.extra;

import java.util.function.Supplier;

public class LazyValue<T> {
  private final Supplier<T> creater;
  private T value = null;
  private boolean calculated = false;

  public LazyValue(Supplier<T> supplier) {
    creater = supplier;
  }

  public synchronized T get() {
    if (calculated == false) {
      value = creater.get();
      calculated = true;
    }
    return value;
  }
}
