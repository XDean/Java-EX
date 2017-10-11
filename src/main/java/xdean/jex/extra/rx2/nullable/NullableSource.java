package xdean.jex.extra.rx2.nullable;

import io.reactivex.Flowable;
import io.reactivex.Observable;

public interface NullableSource<F> {
  <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy);

  default <T> Observable<T> observable(NullPolicy<F, T> policy) {
    return policy(policy).observable();
  }

  default <T> Flowable<T> flowable(NullPolicy<F, T> policy) {
    return policy(policy).flowable();
  }
}