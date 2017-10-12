package xdean.jex.extra.rx2.nullable.impl;

import static xdean.jex.util.function.Predicates.not;
import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.stream.Stream;

import xdean.jex.extra.rx2.nullable.NullHandler;
import xdean.jex.extra.rx2.nullable.NullableObservableFlowable;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

public class NullableArray<F> implements NullableObservableFlowable<F> {
  private final F[] array;

  public NullableArray(F[] array) {
    this.array = array;
  }

  @Override
  public <T> ObservableFlowable<T> handler(NullHandler<F, T> handler) {
    return new Converter<T>().handler(handler);
  }

  public class Converter<T> extends OFWithPolicy<F, T> {
    @Override
    public Observable<T> observable() {
      return Observable.fromArray(get());
    }

    @Override
    public Flowable<T> flowable() {
      return Flowable.fromArray(get());
    }

    @SuppressWarnings("unchecked")
    private T[] get() {
      return (T[]) Stream.of(array)
          .map(handler)
          .filter(not(null))
          .toArray();
    }
  }
}