package cn.xdean.jex.lang;

import com.google.common.collect.Lists;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class ComparatorUtil {
  public static <T> Comparator<T> notSort() {
    return (a, b) -> 0;
  }

  @SafeVarargs
  public static <T> Comparator<T> startWith(T... elements) {
    List<T> list = Lists.reverse(Arrays.asList(elements));
    return (a, b) -> list.indexOf(b) - list.indexOf(a);
  }

  @SafeVarargs
  public static <T> Comparator<T> endWith(T... elements) {
    List<T> list = Arrays.asList(elements);
    return (a, b) -> list.indexOf(a) - list.indexOf(b);
  }
}
