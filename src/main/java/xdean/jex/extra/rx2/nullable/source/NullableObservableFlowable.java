package xdean.jex.extra.rx2.nullable.source;

import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Optional;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;

public interface NullableObservableFlowable<F> extends
    NullableSource<F, ObservableFlowable<F>, ObservableFlowable<Optional<F>>> {
  @Override
  public <T> ObservableFlowable<T> handler(NullHandler<F, T> handler);

  default NullableObservable<F> observable() {
    return new NullableObservable<F>() {
      @Override
      public <T> GenericObservable<T> handler(NullHandler<F, T> handler) {
        return new GenericObservable<>(NullableObservableFlowable.this.handler(handler).observable());
      }
    };
  }

  default NullableFlowable<F> flowable() {
    return new NullableFlowable<F>() {
      @Override
      public <T> GenericFlowable<T> handler(NullHandler<F, T> handler) {
        return new GenericFlowable<>(NullableObservableFlowable.this.handler(handler).flowable());
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
