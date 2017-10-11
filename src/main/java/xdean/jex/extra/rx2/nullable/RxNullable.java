package xdean.jex.extra.rx2.nullable;

import java.util.concurrent.Callable;

import xdean.jex.extra.rx2.nullable.impl.NullableArray;
import xdean.jex.extra.rx2.nullable.impl.NullableCallable;
import xdean.jex.extra.rx2.nullable.impl.NullableIterable;

public class RxNullable {
  public static void main(String[] args) {
    RxNullable.fromArray(1, 2, 3, null, 4, null, 5)
        .observable(NullPolicies.drop())
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

}
