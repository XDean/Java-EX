package xdean.jex.util;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;

public class ComparatorUtil {

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

  @Beta
  @SafeVarargs
  static <T> Comparator<T> relative(T... elements) {
    List<T> list = Arrays.asList(elements);
    return (a, b) -> {
      int ia = list.indexOf(a);
      int ib = list.indexOf(b);
      return ia == -1 || ib == -1 ? 0 : ia - ib;
    };
  }
}
