package cn.xdean.jex.lang.function;

import cn.xdean.jex.lang.collection.Wrapper;
import cn.xdean.jex.lang.function.type.ActionE0;
import cn.xdean.jex.lang.function.type.ActionE1;
import cn.xdean.jex.lang.function.type.FuncE0;
import xdean.codecov.CodecovIgnore;

import java.util.function.*;

@CodecovIgnore
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
    Wrapper<T> w = new Wrapper<>(null);
    c.accept(() -> w.set(s.get()));
    return w.get();
  }

  public static <T, E extends Exception, EE extends Exception> T
      supplierToRunnable(FuncE0<T, E> s, ActionE1<ActionE0<E>, EE> c) throws EE {
    Wrapper<T> w = new Wrapper<>(null);
    c.call(() -> w.set(s.call()));
    return w.get();
  }
}
