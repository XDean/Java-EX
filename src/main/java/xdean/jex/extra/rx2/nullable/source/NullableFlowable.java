package xdean.jex.extra.rx2.nullable.source;

import io.reactivex.Flowable;

import java.util.Optional;

import org.reactivestreams.Subscriber;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;

public interface NullableFlowable<F> extends NullableSource<F, GenericFlowable<F>, GenericFlowable<Optional<F>>> {
  @Override
  <T> GenericFlowable<T> handler(NullHandler<F, T> handler);
}

class GenericFlowable<F> extends Flowable<F> implements Generic<F> {

  private final Flowable<F> actual;

  public GenericFlowable(Flowable<F> actual) {
    this.actual = actual;
  }

  @Override
  protected void subscribeActual(Subscriber<? super F> s) {
    actual.subscribe(s);
  }
}