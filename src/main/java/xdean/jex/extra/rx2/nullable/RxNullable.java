package xdean.jex.extra.rx2.nullable;

import xdean.jex.extra.rx2.nullable.impl.NullableArray;
import io.reactivex.annotations.Experimental;

@Experimental
public class RxNullable {
  public static void main(String[] args) {
    RxNullable.fromArray(1, 2, 3, null, 4, null, 5)
        .policy(NullPolicies.drop())
        .observable()
        .map(i -> i * 2)
        .forEach(System.out::println);
  }

  @SafeVarargs
  public static <F> NullableArray<F> fromArray(F... items) {
    return new NullableArray<F>(items);
  }
}
