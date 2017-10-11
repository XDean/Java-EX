package xdean.jex.extra.rx2.nullable.impl;

import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Optional;
import java.util.concurrent.Callable;

import xdean.jex.extra.rx2.nullable.NullPolicy;
import xdean.jex.extra.rx2.nullable.NullableSource;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

public class NullableCallable<F> implements NullableSource<F> {

  private final Callable<F> callable;

  public NullableCallable(Callable<F> callable) {
    this.callable = callable;
  }

  @Override
  public <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy) {
    return new Converter<T>().policy(policy);
  }

  public class Converter<T> extends OFWithPolicy<F, T> {
    @Override
    public Observable<T> observable() {
      return Observable.fromCallable(get())
          .filter(Optional::isPresent)
          .map(Optional::get);
    }

    @Override
    public Flowable<T> flowable() {
      return Flowable.fromCallable(get())
          .filter(Optional::isPresent)
          .map(Optional::get);
    }

    private Callable<Optional<T>> get() {
      return () -> Optional.ofNullable(policy.apply(callable.call()));
    }
  }
}
