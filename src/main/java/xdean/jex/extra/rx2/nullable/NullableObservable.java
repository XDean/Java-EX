package xdean.jex.extra.rx2.nullable;

import io.reactivex.Observable;

import java.util.Optional;

public interface NullableObservable<F> {
  <T> Observable<T> policy(NullPolicy<F, T> policy);

  default Observable<F> onNullDrop() {
    return policy(NullPolicies.drop());
  }

  default Observable<Optional<F>> onNullWrap() {
    return policy(NullPolicies.wrap());
  }

  default Observable<F> onNullDefault(F defaultValue) {
    return policy(NullPolicies.defaultValue(defaultValue));
  }

  default Observable<F> onNullRun(Runnable action) {
    return policy(NullPolicies.run(action));
  }
}