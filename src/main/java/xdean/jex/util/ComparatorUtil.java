package xdean.jex.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMap.Builder;
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

  @SafeVarargs
  public static <T extends Comparable<T>> Comparator<T> relative(T... elements) {
    return relative(Comparator.<T> naturalOrder(), elements);
  }

  @SafeVarargs
  public static <T> Comparator<T> relative(Comparator<T> comp, T... elements) {
    List<T> origin = Arrays.asList(elements);
    List<T> target = new ArrayList<>(Arrays.asList(elements));
    target.sort(comp);
    if (origin.equals(target)) {
      return comp;
    } else {
      Builder<T, T> builder = ImmutableMap.builder();
      for (int i = 0; i < elements.length; i++) {
        builder.put(origin.get(i), target.get(i));
      }
      ImmutableMap<T, T> map = builder.build();
      return (a, b) -> comp.compare(map.getOrDefault(a, a), map.getOrDefault(b, b));
    }
  }
}
