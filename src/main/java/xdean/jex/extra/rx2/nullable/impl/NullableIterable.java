package xdean.jex.extra.rx2.nullable.impl;

import static xdean.jex.util.function.Predicates.not;
import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import xdean.jex.extra.rx2.nullable.NullPolicy;
import xdean.jex.extra.rx2.nullable.NullableSource;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

public class NullableIterable<F> implements NullableSource<F> {
  private final Iterable<F> iterable;

  public NullableIterable(Iterable<F> iterable) {
    this.iterable = iterable;
  }

  @Override
  public <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy) {
    return new Converter<T>().policy(policy);
  }

  public class Converter<T> extends OFWithPolicy<F, T> {
    @Override
    public Observable<T> observable() {
      return Observable.fromIterable(get());
    }

    @Override
    public Flowable<T> flowable() {
      return Flowable.fromIterable(get());
    }

    private Iterable<T> get() {
      return StreamSupport.stream(iterable.spliterator(), false)
          .map(policy)
          .filter(not(null))
          .collect(Collectors.toList());
    }
  }
}
