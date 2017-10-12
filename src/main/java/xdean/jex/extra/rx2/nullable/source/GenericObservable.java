package xdean.jex.extra.rx2.nullable.source;

import io.reactivex.Observable;
import io.reactivex.Observer;

public class GenericObservable<F> extends Observable<F> implements Generic<F> {

  private final Observable<F> actual;

  public GenericObservable(Observable<F> actual) {
    this.actual = actual;
  }

  @Override
  protected void subscribeActual(Observer<? super F> observer) {
    actual.subscribe(observer);
  }
}