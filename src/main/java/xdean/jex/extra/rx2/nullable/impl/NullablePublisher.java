package xdean.jex.extra.rx2.nullable.impl;

import io.reactivex.Flowable;
import io.reactivex.Observable;
import io.reactivex.subscribers.DefaultSubscriber;

import org.reactivestreams.Publisher;

import xdean.jex.extra.rx2.nullable.NullPolicy;
import xdean.jex.extra.rx2.nullable.NullableSource;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

public class NullablePublisher<F> implements NullableSource<F> {
  private final Publisher<F> publisher;

  public NullablePublisher(Publisher<F> publisher) {
    this.publisher = publisher;
  }

  @Override
  public <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy) {
    return new Converter<T>().policy(policy);
  }

  public class Converter<T> extends OFWithPolicy<F, T> {
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
          T t = policy.apply(f);
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
