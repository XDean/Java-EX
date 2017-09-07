package xdean.jex.util.function;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import xdean.jex.extra.Wrapper;
import xdean.jex.extra.function.ConsumerThrow;
import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.extra.function.SupplierThrow;

public class FunctionAdapter {

  public static <T> Supplier<T> supplier(T t) {
    return () -> t;
  }

  public static <T> UnaryOperator<T> function(Consumer<T> c) {
    return t -> {
      c.accept(t);
      return t;
    };
  }

  public static <T> Consumer<T> consumer(Runnable r) {
    return t -> r.run();
  }

  public static <A, B> BiConsumer<A, B> consumeFirst(Consumer<A> c) {
    return (a, b) -> c.accept(a);
  }

  public static <A, B> BiConsumer<A, B> consumeSecond(Consumer<B> c) {
    return (a, b) -> c.accept(b);
  }

  public static <A, B> BiFunction<A, B, A> functionFirst(BiConsumer<A, B> bc) {
    return (a, b) -> {
      bc.accept(a, b);
      return a;
    };
  }

  public static <A, B> BiFunction<A, B, B> functionSecond(BiConsumer<A, B> bc) {
    return (a, b) -> {
      bc.accept(a, b);
      return b;
    };
  }

  /**
   * @param s
   * @param c
   * @return the supplier's product
   */
  public static <T> T supplierToRunnable(Supplier<T> s, Consumer<Runnable> c) {
    Wrapper<T> w = new Wrapper<T>(null);
    c.accept(() -> w.set(s.get()));
    return w.get();
  }

  public static <T, E extends Exception, EE extends Exception> T
      supplierToRunnable(SupplierThrow<T, E> s, ConsumerThrow<RunnableThrow<E>, EE> c) throws EE {
    Wrapper<T> w = new Wrapper<T>(null);
    c.accept(() -> w.set(s.get()));
    return w.get();
  }
}
