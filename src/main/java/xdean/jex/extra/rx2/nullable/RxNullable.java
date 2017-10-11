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

public class RxNullable {
  public static void main(String[] args) {
    RxNullable.fromArray(1, 2, 3, null, 4, null, 5)
        .onNullDrop()
        .observable()
        .map(i -> i * 2)
        .forEach(System.out::println);
  }

  @SafeVarargs
  public static <F> NullableSource<F> fromArray(F... items) {
    return new NullableArray<>(items);
  }

  public static <F> NullableSource<F> fromIterable(Iterable<F> iterable) {
    return new NullableIterable<>(iterable);
  }

  public static <F> NullableSource<F> fromCallable(Callable<F> callable) {
    return new NullableCallable<>(callable);
  }

  public static <F> NullableSource<F> fromPublisher(Publisher<F> publisher) {
    return new NullablePublisher<>(publisher);
  }

  public static <F> NullableSource<F> fromFuture(Future<F> future) {
    return new NullableFuture<>(future);
  }

  public static <F> NullableSource<F> fromFuture(Future<F> future, long timeout, TimeUnit unit) {
    return new NullableFuture<>(future, timeout, unit);
  }
}
