package cn.xdean.jex.lang.math;

import io.reactivex.Flowable;

import java.util.Arrays;
import java.util.List;

public class CartesianProduct {

  public static Flowable<int[]> cartesianProduct(Flowable<Flowable<Integer>> sources) {
    return sources.toList().<int[]> flatMapPublisher(list -> cartesian(list));
  }

  /**
   *
   * @author akarnokd@StackOverflow
   * @param sources
   * @return
   */
  public static Flowable<int[]> cartesian(List<Flowable<Integer>> sources) {
    if (sources.size() == 0) {
      return Flowable.empty();
    }
    Flowable<int[]> main = Flowable.just(new int[0]);
    for (int i = 0; i < sources.size(); i++) {
      int j = i;
      Flowable<Integer> o = sources.get(i).cache();
      main = main.<int[]> flatMap(v -> o.map(w -> {
        int[] arr = Arrays.copyOf(v, j + 1);
        arr[j] = w;
        return arr;
      }));
    }

    return main;
  }
}