package xdean.jex.util;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import com.google.common.collect.Lists;

import xdean.jex.extra.RelativeComparator;

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

  @SafeVarargs
  public static <T extends Comparable<T>> Comparator<T> relative(T... elements) {
    return relative(Comparator.<T> naturalOrder(), elements);
  }

  @SafeVarargs
  public static <T> Comparator<T> relative(Comparator<T> comp, T... elements) {
    return RelativeComparator.of(comp, elements);
  }
}
