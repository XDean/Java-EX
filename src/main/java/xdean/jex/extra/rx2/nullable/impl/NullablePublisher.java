package xdean.jex.extra.rx2.nullable.impl;

import io.reactivex.Flowable;
import io.reactivex.Observable;
import io.reactivex.subscribers.DefaultSubscriber;

import org.reactivestreams.Publisher;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;
import xdean.jex.extra.rx2.nullable.source.NullableObservableFlowable;
import xdean.jex.extra.rx2.nullable.source.ObservableFlowable;

public class NullablePublisher<F> implements NullableObservableFlowable<F> {
  private final Publisher<F> publisher;

  public NullablePublisher(Publisher<F> publisher) {
    this.publisher = publisher;
  }

  @Override
  public <T> ObservableFlowable<T> handler(NullHandler<F, T> handler) {
    return new Converter<T>().handler(handler);
  }

  public class Converter<T> extends OFWithHandler<F, T> {
    @Override
    public Observable<T> observable() {
      return Observable.fromPublisher(get());
    }

    @Override
    public Flowable<T> flowable() {
      return Flowable.fromPublisher(get());
    }

    private Publisher<T> get() {
      return actual -> publisher.subscribe(new DefaultSubscriber<F>() {
        @Override
        protected void onStart() {
        }

        @Override
        public void onNext(F f) {
          T t = handler.apply(f);
          if (t == null) {
            request(1);
          } else {
            actual.onNext(t);
          }
        }

        @Override
        public void onError(Throwable t) {
          actual.onError(t);
        }

        @Override
        public void onComplete() {
          actual.onComplete();
        }
      });
    }
  }
}
