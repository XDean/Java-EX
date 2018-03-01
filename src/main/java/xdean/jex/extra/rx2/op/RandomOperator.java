package xdean.jex.extra.rx2.op;

import static io.reactivex.internal.util.BackpressureHelper.add;
import static io.reactivex.internal.util.BackpressureHelper.produced;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;

import io.reactivex.FlowableOperator;
import io.reactivex.FlowableSubscriber;
import io.reactivex.ObservableOperator;
import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import io.reactivex.internal.subscriptions.SubscriptionHelper;

public class RandomOperator<T> {

  private static final int DEFAULT_CACHE = 128;

  public static <T> ObservableOperator<T, T> observable() {
    return observable(DEFAULT_CACHE);
  }

  public static <T> ObservableOperator<T, T> observable(int cacheSize) {
    return actual -> new RandomObserver<>(actual, cacheSize);
  }

  public static <T> FlowableOperator<T, T> flowable() {
    return flowable(DEFAULT_CACHE);
  }

  public static <T> FlowableOperator<T, T> flowable(int cacheSize) {
    return actual -> new RandomSubscriber<>(actual, cacheSize);
  }

  private static final class RandomObserver<T> implements Observer<T> {
    int cacheSize;
    List<T> elements;
    Observer<? super T> actual;

    public RandomObserver(Observer<? super T> actual, int cacheSize) {
      this.actual = actual;
      this.cacheSize = cacheSize;
      elements = new ArrayList<>(cacheSize);
    }

    @Override
    public void onSubscribe(Disposable d) {
      actual.onSubscribe(d);
    }

    @Override
    public void onNext(T t) {
      if (elements.size() == cacheSize) {
        emitOne();
      }
      elements.add(t);
    }

    @Override
    public void onError(Throwable e) {
      actual.onError(e);
    }

    @Override
    public void onComplete() {
      emitAll();
      actual.onComplete();
    }

    private void emitOne() {
      actual.onNext(elements.remove((int) (Math.random() * elements.size())));
    }

    private void emitAll() {
      while (!elements.isEmpty()) {
        emitOne();
      }
    }
  }

  private static final class RandomSubscriber<T> extends AtomicLong implements FlowableSubscriber<T>, Subscription {
    int cacheSize;
    List<T> elements;
    Subscriber<? super T> actual;
    Subscription s;

    public RandomSubscriber(Subscriber<? super T> actual, int cacheSize) {
      elements = new ArrayList<>(cacheSize);
      this.cacheSize = cacheSize;
      this.actual = actual;
    }

    @Override
    public void onSubscribe(Subscription s) {
      if (SubscriptionHelper.validate(this.s, s)) {
        this.s = s;
        actual.onSubscribe(this);
      }
    }

    @Override
    public void onNext(T t) {
      elements.add(t);
      if (elements.size() == cacheSize) {
        emitOne();
      }
      if (get() > 0) {
        s.request(1);
      }
    }

    @Override
    public void onError(Throwable e) {
      actual.onError(e);
    }

    @Override
    public void onComplete() {
      emitAll();
      actual.onComplete();
    }

    @Override
    public void request(long n) {
      if (add(this, n) == 0) {
        s.request(n);
      }
    }

    @Override
    public void cancel() {
      s.cancel();
    }

    private void emitOne() {
      actual.onNext(elements.remove((int) (Math.random() * elements.size())));
      produced(this, 1);
    }

    private void emitAll() {
      while (!elements.isEmpty() && get() > 0) {
        emitOne();
      }
    }
  }
}
