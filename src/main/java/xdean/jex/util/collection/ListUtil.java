package xdean.jex.util.collection;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ListUtil {

  public <T> T lastGet(List<? extends T> list, int index) {
    if (list.size() < index) {
      return null;
    }
    return list.get(list.size() - 1 - index);
  }

  public <F, T> List<T> map(List<? extends F> list, Function<F, T> func) {
    return list.stream().map(func).collect(Collectors.toList());
  }

  public <T> void forEach(List<? extends T> list, BiConsumer<T, Integer> consumer) {
    for (int i = 0; i < list.size(); i++) {
      consumer.accept(list.get(i), i);
    }
  }

  public <T> void addIfAbsent(List<? super T> list, T t) {
    if (list.contains(t) == false) {
      list.add(t);
    }
  }
}
