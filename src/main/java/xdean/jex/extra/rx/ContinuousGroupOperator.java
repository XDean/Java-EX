package xdean.jex.extra.rx;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.experimental.FieldDefaults;
import rx.Observable.Operator;
import rx.Subscriber;
import xdean.jex.extra.Pair;

/**
 * TODO
 * 
 * @author XDean
 *
 */
@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
@AllArgsConstructor
public class ContinuousGroupOperator<K, T> implements Operator<Pair<K, List<T>>, T> {

  Function<T, ? extends K> keySelector;

  @Override
  public Subscriber<? super T> call(Subscriber<? super Pair<K, List<T>>> s) {
    ContinuousGroupSubscriber ts = new ContinuousGroupSubscriber(s);
    s.add(ts);
    return ts;
  }

  private class ContinuousGroupSubscriber extends Subscriber<T> {

    private final Subscriber<? super Pair<K, List<T>>> actual;
    private K key;
    private List<T> list;

    public ContinuousGroupSubscriber(Subscriber<? super Pair<K, List<T>>> actual) {
      this.actual = actual;
    }

    @Override
    public void onNext(T next) {
      K nextKey = keySelector.apply(next);
      if (nextKey.equals(key)) {
        if (list == null) {
          list = new ArrayList<>();
        }
        list.add(next);
      } else {
        if (list != null) {
          actual.onNext(Pair.of(key, list));
        }
        list = new ArrayList<>();
      }
      key = nextKey;
    }

    @Override
    public void onCompleted() {
      if (list != null) {
        actual.onNext(Pair.of(key, list));
      }
      actual.onCompleted();
    }

    @Override
    public void onError(Throwable e) {
      actual.onError(e);
    }
  }
}
