package cn.xdean.jex.lang.function;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

public class Predicates {

  public static <T> Predicate<T> is(T t) {
    return o -> o == t;
  }

  public static <T> Predicate<T> not(T t) {
    return o -> o != t;
  }

  public static <T> Predicate<T> isEquals(T t) {
    return o -> Objects.equals(o, t);
  }

  public static <T> Predicate<T> notEquals(T t) {
    return o -> !Objects.equals(o, t);
  }

  public static <F, T> Predicate<F> its(Function<F, T> keySelector, Predicate<T> p) {
    return o -> p.test(keySelector.apply(o));
  }
}
