package xdean.jex.extra.rx2.nullable;

import java.util.Optional;

import io.reactivex.Flowable;
import io.reactivex.Observable;

public interface NullableSource<F> {
  <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy);

  default ObservableFlowable<F> drop() {
    return policy(NullPolicies.drop());
  }

  default ObservableFlowable<Optional<F>> wrap() {
    return policy(NullPolicies.wrap());
  }

  default ObservableFlowable<F> defaultValue(F defaultValue) {
    return policy(NullPolicies.defaultValue(defaultValue));
  }

  default <T> Observable<T> observable(NullPolicy<F, T> policy) {
    return policy(policy).observable();
  }

  default <T> Flowable<T> flowable(NullPolicy<F, T> policy) {
    return policy(policy).flowable();
  }
}