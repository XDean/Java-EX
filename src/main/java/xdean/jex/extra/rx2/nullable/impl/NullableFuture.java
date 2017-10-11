package xdean.jex.extra.rx2.nullable.impl;

import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import xdean.jex.extra.rx2.nullable.NullPolicy;
import xdean.jex.extra.rx2.nullable.NullableSource;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

public class NullableFuture<F> implements NullableSource<F> {
  private final Future<F> future;
  private final long timeout;
  private final TimeUnit unit;

  public NullableFuture(Future<F> future) {
    this.future = future;
    this.timeout = 0L;
    this.unit = null;
  }

  public NullableFuture(Future<F> future, long timeout, TimeUnit unit) {
    this.future = future;
    this.timeout = timeout;
    this.unit = unit;
  }

  @Override
  public <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy) {
    return new Converter<T>().policy(policy);
  }

  public class Converter<T> extends OFWithPolicy<F, T> {
    @Override
    public Observable<T> observable() {
      return Observable.fromFuture(get(), timeout, unit)
          .filter(Optional::isPresent)
          .map(Optional::get);
    }

    @Override
    public Flowable<T> flowable() {
      return Flowable.fromFuture(get(), timeout, unit)
          .filter(Optional::isPresent)
          .map(Optional::get);
    }

    private Future<Optional<T>> get() {
      return new Future<Optional<T>>() {
        @Override
        public boolean cancel(boolean mayInterruptIfRunning) {
          return future.cancel(mayInterruptIfRunning);
        }

        @Override
        public boolean isCancelled() {
          return future.isCancelled();
        }

        @Override
        public boolean isDone() {
          return future.isDone();
        }

        @Override
        public Optional<T> get() throws InterruptedException, ExecutionException {
          return convert(future.get());
        }

        @Override
        public Optional<T> get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException,
            TimeoutException {
          return convert(future.get(timeout, unit));
        }

        private Optional<T> convert(F f) {
          return Optional.ofNullable(policy.apply(f));
        }
      };
    }
  }
}
