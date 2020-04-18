package cn.xdean.jex.thrid.rxjava;

import io.reactivex.functions.*;

import java.util.concurrent.Callable;

public class RxFunctions {
  public static Action rx(java.lang.Runnable java) {
    return java::run;
  }

  public static <T> Consumer<T> rx(java.util.function.Consumer<T> java) {
    return java::accept;
  }

  public static <T> Callable<T> rx(java.util.function.Supplier<T> java) {
    return java::get;
  }

  public static <T, R> Function<T, R> rx(java.util.function.Function<T, R> java) {
    return java::apply;
  }

  public static <T> Predicate<T> rx(java.util.function.Predicate<T> java) {
    return java::test;
  }

  public static <T1, T2> BiConsumer<T1, T2> rx(java.util.function.BiConsumer<T1, T2> java) {
    return java::accept;
  }
}
