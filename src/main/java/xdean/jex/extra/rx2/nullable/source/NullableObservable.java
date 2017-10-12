package xdean.jex.extra.rx2.nullable.source;

import io.reactivex.Observable;
import io.reactivex.Observer;

import java.util.Optional;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;

public interface NullableObservable<F> extends NullableSource<F, GenericObservable<F>, GenericObservable<Optional<F>>> {
  @Override
  <T> GenericObservable<T> handler(NullHandler<F, T> handler);
}

class GenericObservable<F> extends Observable<F> implements Generic<F> {

  private final Observable<F> actual;

  public GenericObservable(Observable<F> actual) {
    this.actual = actual;
  }

  @Override
  protected void subscribeActual(Observer<? super F> observer) {
    actual.subscribe(observer);
  }
}