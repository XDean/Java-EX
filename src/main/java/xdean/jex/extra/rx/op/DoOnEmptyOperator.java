package xdean.jex.extra.rx.op;

import java.util.concurrent.atomic.AtomicBoolean;

import rx.Subscriber;

public class DoOnEmptyOperator<T> extends SimpleOperator<T, T> {

  public static <T> DoOnEmptyOperator<T> create(Runnable runnable) {
    return new DoOnEmptyOperator<>(runnable);
  }

  private DoOnEmptyOperator(Runnable runnable) {
    super(actual -> new Subscriber<T>() {
      AtomicBoolean isEmpty = new AtomicBoolean(true);

      @Override
      public void onNext(T t) {
        isEmpty.compareAndSet(true, false);
        actual.onNext(t);
      }

      @Override
      public void onCompleted() {
        if (isEmpty.get()) {
          runnable.run();
        }
        actual.onCompleted();
      }

      @Override
      public void onError(Throwable e) {
        actual.onError(e);
      }
    });
  }

}
