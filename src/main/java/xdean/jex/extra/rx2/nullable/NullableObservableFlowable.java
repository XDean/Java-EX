package xdean.jex.extra.rx2.nullable;

import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Optional;

public interface NullableObservableFlowable<F> {
  <T> ObservableFlowable<T> handler(NullHandler<F, T> handler);

  default ObservableFlowable<F> onNullDrop() {
    return handler(NullHandlers.drop());
  }

  default ObservableFlowable<Optional<F>> onNullWrap() {
    return handler(NullHandlers.wrap());
  }

  default ObservableFlowable<F> onNullDefault(F defaultValue) {
    return handler(NullHandlers.defaultValue(defaultValue));
  }

  default ObservableFlowable<F> onNullRun(Runnable action) {
    return handler(NullHandlers.run(action));
  }

  default NullableObservable<F> observable() {
    return new NullableObservable<F>() {
      @Override
      public <T> Observable<T> handler(NullHandler<F, T> handler) {
        return NullableObservableFlowable.this.handler(handler).observable();
      }
    };
  }

  default NullableFlowable<F> flowable() {
    return new NullableFlowable<F>() {
      @Override
      public <T> Flowable<T> handler(NullHandler<F, T> handler) {
        return NullableObservableFlowable.this.handler(handler).flowable();
      }
    };
  }

  default <T> Observable<T> observable(NullHandler<F, T> handler) {
    return handler(handler).observable();
  }

  default <T> Flowable<T> flowable(NullHandler<F, T> handler) {
    return handler(handler).flowable();
  }
}