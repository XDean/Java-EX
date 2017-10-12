package xdean.jex.extra.rx2.nullable.impl;

import static xdean.jex.util.function.Predicates.not;
import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;
import xdean.jex.extra.rx2.nullable.source.NullableObservableFlowable;
import xdean.jex.extra.rx2.nullable.source.ObservableFlowable;

public class NullableIterable<F> implements NullableObservableFlowable<F> {
  private final Iterable<F> iterable;

  public NullableIterable(Iterable<F> iterable) {
    this.iterable = iterable;
  }

  @Override
  public <T> ObservableFlowable<T> handler(NullHandler<F, T> handler) {
    return new Converter<T>().handler(handler);
  }

  public class Converter<T> extends OFWithHandler<F, T> {
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
          .map(handler)
          .filter(not(null))
          .collect(Collectors.toList());
    }
  }
}
