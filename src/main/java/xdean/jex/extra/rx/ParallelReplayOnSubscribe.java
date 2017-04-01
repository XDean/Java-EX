package xdean.jex.extra.rx;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import lombok.AccessLevel;
import lombok.experimental.FieldDefaults;
import lombok.experimental.NonFinal;
import rx.Observable;
import rx.Observer;
import rx.Subscriber;
import rx.observables.SyncOnSubscribe;

import com.google.common.collect.ImmutableList;

/**
 * 
 * Replay the source Observable, but do not switch thread.
 * 
 * @author XDean
 *
 * @param <T>
 */
@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
public class ParallelReplayOnSubscribe<T> extends SyncOnSubscribe<Integer, T> {

  public static <T> Observable<T> create(Observable<T> source) {
    return new ParallelReplayOnSubscribe<>(source).create();
  }
  
  public Observable<T> create() {
    return Observable.create(this);
  }

  @NonFinal
  volatile List<T> list = Collections.synchronizedList(new ArrayList<>());
  AtomicInteger subscribeCount = new AtomicInteger();
  AtomicBoolean ended = new AtomicBoolean(false);
  InnerSubscriber subscriber;

  AtomicBoolean completed = new AtomicBoolean(false);
  AtomicReference<Throwable> error = new AtomicReference<>();

  public ParallelReplayOnSubscribe(Observable<T> source) {
    subscriber = new InnerSubscriber();
    source.subscribe(subscriber);
  }

  @Override
  protected Integer generateState() {
    if (ended.get()) {
      throw new IllegalStateException("Can't subscribe an ended observable");
    }
    subscribeCount.incrementAndGet();
    return 0;
  }

  @Override
  protected Integer next(final Integer state, final Observer<? super T> observer) {
    if (error.get() != null) {
      observer.onError(error.get());
      return -1;
    } else if (list.size() == state && completed.get()) {
      observer.onCompleted();
      return state;
    } else if (list.size() <= state) {
      subscriber.requestPublic(1);
      return state;
    } else {
      observer.onNext(list.get(state));
      return state + 1;
    }
  }

  @Override
  protected void onUnsubscribe(Integer state) {
    subscribeCount.decrementAndGet();
  }

  // TODO release useless elements when ended.
  @Deprecated
  public void end() {
    ended.compareAndSet(false, true);
  }

  private final class InnerSubscriber extends Subscriber<T> {
    @Override
    public void onStart() {
      request(1);
    }

    @Override
    public void onCompleted() {
      completed.compareAndSet(false, true);
      list = ImmutableList.copyOf(list);
    }

    @Override
    public void onError(Throwable e) {
      error.compareAndSet(null, e);
    }

    @Override
    public void onNext(T t) {
      list.add(t);
    }

    void requestPublic(int i) {
      request(i);
    }
  }
}
