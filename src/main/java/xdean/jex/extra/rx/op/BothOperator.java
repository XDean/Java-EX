package xdean.jex.extra.rx.op;

import static xdean.jex.util.cache.CacheUtil.*;

import java.util.Optional;

import rx.Subscriber;
import xdean.jex.extra.Pair;

public class BothOperator<T> extends NormalOperator<Pair<T, T>, T> {

  @Override
  protected void onNext(Subscriber<? super Pair<T, T>> actual, T t) {
    Optional<T> old = get(this, actual);
    if (old.isPresent()) {
      actual.onNext(Pair.of(old.get(), t));
    }
    set(this, actual, t);
  }
}
