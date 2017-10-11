package xdean.jex.extra.rx2;

import io.reactivex.Observable;
import io.reactivex.annotations.Experimental;

import java.util.Arrays;
import java.util.concurrent.Callable;
import java.util.stream.StreamSupport;

import xdean.jex.extra.GetSelf;

@Experimental
public class NullableSource {
  public static void main(String[] args) {
    NullableSource.<Integer, Integer> observable()
        .policy(NullPolicies.drop())
        .fromArray(1, 2, 3, null, 4, null, 5)
        .map(i -> i * 2)
        .forEach(System.out::println);
  }

  public interface NullPolicy<F, T> {
    Iterable<T> handle(Iterable<F> iterator);
  }

  public static class NullPolicies {
    public static <T> NullPolicy<T, T> drop() {
      return i -> () -> StreamSupport.stream(i.spliterator(), false)
          .filter(t -> t != null)
          .iterator();
    }
  }

  public static <F, T> NullableObservable<F, T> observable() {
    return new NullableObservable<>();
  }

  public static NullableFlowable flowable() {
    return new NullableFlowable();
  }

  public static abstract class Nullable<F, T, S extends Nullable<F, T, S>> implements GetSelf<S> {
    protected NullPolicy<F, T> policy;

    public S policy(NullPolicy<F, T> policy) {
      this.policy = policy;
      return getSelf();
    }
  }

  public static class NullableObservable<F, T> extends Nullable<F, T, NullableObservable<F, T>> {
    @SuppressWarnings("unchecked")
    public Observable<T> fromArray(F... items) {
      return Observable.fromIterable(policy.handle(Arrays.asList(items)));
    }

    public Observable<T> fromCallable(Callable<F> callable) {
      return Observable.fromCallable(() -> Observable.fromIterable(policy.handle(Arrays.asList(callable.call()))))
          .flatMap(o -> o);
    }
  }

  public static class NullableFlowable {

  }
}
