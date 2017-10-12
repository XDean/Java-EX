package xdean.jex.extra.rx2.nullable;

import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Optional;

public interface NullableSource<F> {
  <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy);

  default ObservableFlowable<F> onNullDrop() {
    return policy(NullPolicies.drop());
  }

  default ObservableFlowable<Optional<F>> onNullWrap() {
    return policy(NullPolicies.wrap());
  }

  default ObservableFlowable<F> onNullDefault(F defaultValue) {
    return policy(NullPolicies.defaultValue(defaultValue));
  }

  default ObservableFlowable<F> onNullRun(Runnable action) {
    return policy(NullPolicies.run(action));
  }

  default NullableObservable<F> observable() {
    return new NullableObservable<F>() {
      @Override
      public <T> Observable<T> policy(NullPolicy<F, T> policy) {
        return NullableSource.this.policy(policy).observable();
      }
    };
  }

  default NullableFlowable<F> flowable() {
    return new NullableFlowable<F>() {
      @Override
      public <T> Flowable<T> policy(NullPolicy<F, T> policy) {
        return NullableSource.this.policy(policy).flowable();
      }
    };
  }

  default <T> Observable<T> observable(NullPolicy<F, T> policy) {
    return policy(policy).observable();
  }

  default <T> Flowable<T> flowable(NullPolicy<F, T> policy) {
    return policy(policy).flowable();
  }
}