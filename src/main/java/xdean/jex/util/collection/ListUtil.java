package xdean.jex.util.collection;

import java.util.List;
import java.util.function.BiConsumer;

public class ListUtil {

  public static <T> T lastGet(List<? extends T> list, int index) {
    if (list.size() < index) {
      return null;
    }
    return list.get(list.size() - 1 - index);
  }

  public static <T> void forEach(List<? extends T> list, BiConsumer<T, Integer> consumer) {
    for (int i = 0; i < list.size(); i++) {
      consumer.accept(list.get(i), i);
    }
  }

  public static <T> boolean addIfAbsent(List<? super T> list, T t) {
    if (list.contains(t) == false) {
      return list.add(t);
    }
    return false;
  }
}
