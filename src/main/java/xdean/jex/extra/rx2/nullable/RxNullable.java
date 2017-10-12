package xdean.jex.extra.rx2.nullable;

import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.reactivestreams.Publisher;

import xdean.jex.extra.rx2.nullable.impl.NullableArray;
import xdean.jex.extra.rx2.nullable.impl.NullableCallable;
import xdean.jex.extra.rx2.nullable.impl.NullableFuture;
import xdean.jex.extra.rx2.nullable.impl.NullableIterable;
import xdean.jex.extra.rx2.nullable.impl.NullablePublisher;
import xdean.jex.extra.rx2.nullable.source.NullableObservableFlowable;

public interface RxNullable {
  @SafeVarargs
  static <F> NullableObservableFlowable<F> fromArray(F... items) {
    return new NullableArray<>(items);
  }

  static <F> NullableObservableFlowable<F> fromIterable(Iterable<F> iterable) {
    return new NullableIterable<>(iterable);
  }

  static <F> NullableObservableFlowable<F> fromCallable(Callable<F> callable) {
    return new NullableCallable<>(callable);
  }

  static <F> NullableObservableFlowable<F> fromPublisher(Publisher<F> publisher) {
    return new NullablePublisher<>(publisher);
  }

  static <F> NullableObservableFlowable<F> fromFuture(Future<F> future) {
    return new NullableFuture<>(future);
  }

  static <F> NullableObservableFlowable<F> fromFuture(Future<F> future, long timeout, TimeUnit unit) {
    return new NullableFuture<>(future, timeout, unit);
  }
}
