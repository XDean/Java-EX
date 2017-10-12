package xdean.jex.extra.rx2.nullable.source;

import io.reactivex.Flowable;

import org.reactivestreams.Subscriber;

public class GenericFlowable<F> extends Flowable<F> implements Generic<F> {

  private final Flowable<F> actual;

  public GenericFlowable(Flowable<F> actual) {
    this.actual = actual;
  }

  @Override
  protected void subscribeActual(Subscriber<? super F> s) {
    actual.subscribe(s);
  }
}