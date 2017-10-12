package xdean.jex.extra.rx2.nullable;

import io.reactivex.Flowable;

import java.util.Optional;

public interface NullableFlowable<F> {
  <T> Flowable<T> handler(NullHandler<F, T> handler);

  default Flowable<F> onNullDrop() {
    return handler(NullHandlers.drop());
  }

  default Flowable<Optional<F>> onNullWrap() {
    return handler(NullHandlers.wrap());
  }

  default Flowable<F> onNullDefault(F defaultValue) {
    return handler(NullHandlers.defaultValue(defaultValue));
  }

  default Flowable<F> onNullRun(Runnable action) {
    return handler(NullHandlers.run(action));
  }
}