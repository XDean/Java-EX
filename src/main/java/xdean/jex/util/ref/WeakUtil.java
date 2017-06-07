package xdean.jex.util.ref;

import java.lang.ref.WeakReference;
import java.util.function.Consumer;

import lombok.experimental.UtilityClass;

@UtilityClass
public class WeakUtil {
  public <T> Runnable weak(T t, Consumer<T> con) {
    Weak<T> weak = new Weak<T>(t);
    return () -> weak.doIfPresent(con);
  }

  private class Weak<T> extends WeakReference<T> {
    Weak(T referent) {
      super(referent);
    }

    void doIfPresent(Consumer<T> consumer) {
      T t = get();
      if (t != null) {
        consumer.accept(t);
      }
    }
  }
}
