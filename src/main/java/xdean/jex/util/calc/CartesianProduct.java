package xdean.jex.util.calc;

import java.util.Arrays;
import java.util.List;

import rx.Observable;
import rx.Observer;
import rx.observables.SyncOnSubscribe;
import xdean.jex.util.collection.ListUtil;

public class CartesianProduct {

  static Observable<int[]> cartesianProduct(Observable<Observable<Integer>> sources) {
    return sources.toList().flatMap(list -> cartesian(list));
  }

  /**
   * 
   * @author akarnokd@StackOverflow
   * @param sources
   * @return
   */
  private static Observable<int[]> cartesian(List<Observable<Integer>> sources) {
    if (sources.size() == 0) {
      return Observable.empty();
    }
    Observable<int[]> main = Observable.just(new int[0]);
    for (int i = 0; i < sources.size(); i++) {
      int j = i;
      Observable<Integer> o = sources.get(i).cache();
      main = main.flatMap(v -> o.map(w -> {
        int[] arr = Arrays.copyOf(v, j + 1);
        arr[j] = w;
        return arr;
      }));
    }

    return main;
  }

  /**
   * 
   * @author XDean
   * @param obs
   * @return
   */
  @SuppressWarnings("unused")
  @Deprecated
  private Observable<int[]> myCartesianProduct(Observable<Observable<Integer>> obs) {
    List<int[]> list = obs.map(ob -> toArray(ob)).toList().toBlocking().last();
    return Observable.create(new SyncOnSubscribe<int[], int[]>() {
      @Override
      protected int[] generateState() {
        int[] array = new int[list.size()];
        Arrays.fill(array, 0);
        return array;
      }

      @Override
      protected int[] next(int[] state, Observer<? super int[]> observer) {
        int[] next = new int[list.size()];
        for (int i = 0; i < next.length; i++) {
          next[i] = list.get(i)[state[i]];
        }
        observer.onNext(next);
        state[state.length - 1]++;
        for (int i = state.length - 1; i >= 0; i--) {
          int delta = list.get(i).length - state[i];
          if (delta > 0) {
            break;
          } else if (delta == 0) {
            state[i] = 0;
            if (i == 0) {
              observer.onCompleted();
              break;
            }
            state[i - 1]++;
          }
        }
        return state;
      }
    });
  }

  private int[] toArray(Observable<Integer> ob) {
    List<Integer> list = ob.toList().toBlocking().last();
    int[] array = new int[list.size()];
    ListUtil.forEach(list, (t, n) -> array[n] = t);
    return array;
  }
}