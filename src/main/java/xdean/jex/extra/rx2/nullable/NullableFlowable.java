package xdean.jex.extra.rx2.nullable;

import io.reactivex.Flowable;

import java.util.Optional;

public interface NullableFlowable<F> {
  <T> Flowable<T> policy(NullPolicy<F, T> policy);

  default Flowable<F> onNullDrop() {
    return policy(NullPolicies.drop());
  }

  default Flowable<Optional<F>> onNullWrap() {
    return policy(NullPolicies.wrap());
  }

  default Flowable<F> onNullDefault(F defaultValue) {
    return policy(NullPolicies.defaultValue(defaultValue));
  }

  default Flowable<F> onNullRun(Runnable action) {
    return policy(NullPolicies.run(action));
  }
}