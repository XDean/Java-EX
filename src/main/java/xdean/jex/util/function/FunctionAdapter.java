package xdean.jex.util.function;

import java.util.function.Consumer;
import java.util.function.Supplier;

import lombok.experimental.UtilityClass;
import xdean.jex.extra.Wrapper;
import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.extra.function.SupplierThrow;

@UtilityClass
public class FunctionAdapter {
  /**
   * @param s
   * @param c
   * @return the supplier's product
   */
  public <T> T supplierToRunnable(Supplier<T> s, Consumer<Runnable> c) {
    Wrapper<T> w = new Wrapper<T>(null);
    c.accept(() -> w.set(s.get()));
    return w.get();
  }

  public <T, E extends Throwable> T supplierToRunnable(SupplierThrow<T, E> s, Consumer<RunnableThrow<E>> c) {
    Wrapper<T> w = new Wrapper<T>(null);
    c.accept(() -> w.set(s.get()));
    return w.get();
  }
}
