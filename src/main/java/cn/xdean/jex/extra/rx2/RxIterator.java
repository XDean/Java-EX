package cn.xdean.jex.extra.rx2;

import static cn.xdean.jex.util.lang.ExceptionUtil.uncheck;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.LinkedBlockingQueue;

import org.reactivestreams.Subscription;

import io.reactivex.Flowable;
import io.reactivex.Notification;
import io.reactivex.Observable;
import io.reactivex.Scheduler;
import io.reactivex.annotations.SchedulerSupport;
import io.reactivex.functions.Function;
import io.reactivex.schedulers.Schedulers;
import cn.xdean.jex.util.lang.ExceptionUtil;

/**
 * Convert RxStream to Iterator.
 *
 * @author XDean, Michael@stackoverflow
 *
 */
public class RxIterator {
  public static <T> Function<Flowable<T>, Iterator<T>> flowableIterator() {
    return o -> toIterator(o);
  }

  public static <T> Iterator<T> toIterator(Flowable<T> ob) {
    return new FlowableIterator<>(ob);
  }

  public static <T> Function<Observable<T>, Iterator<T>> observableIterator(Scheduler scheduler) {
    return o -> toIterator(o, scheduler);
  }

  public static <T> Function<Observable<T>, Iterator<T>> observableIterator() {
    return o -> toIterator(o);
  }

  public static <T> Iterator<T> toIterator(Observable<T> ob, Scheduler scheduler) {
    return new ObservableIterator<>(ob, scheduler);
  }

  @SchedulerSupport(SchedulerSupport.IO)
  public static <T> Iterator<T> toIterator(Observable<T> ob) {
    return toIterator(ob, Schedulers.io());
  }

  public static final class ObservableIterator<T> implements Iterator<T> {
    private LinkedBlockingQueue<Notification<T>> queue = new LinkedBlockingQueue<>();
    private Notification<T> next = null;
    private boolean completed = false;

    public ObservableIterator(Observable<T> source, Scheduler scheduler) {
      source
          .materialize()
          .subscribeOn(scheduler)
          .subscribe(queue::put);
    }

    @Override
    public boolean hasNext() {
      calcNext();
      return !completed;
    }

    @Override
    public T next() {
      calcNext();
      if (next == null) {
        throw new NoSuchElementException();
      }
      T t = next.getValue();
      next = null;
      return t;
    }

    private void calcNext() {
      if (completed) {
        return;
      }
      if (next == null) {
        Notification<T> take = ExceptionUtil.uncheck(queue::take);
        if (take.isOnNext()) {
          next = take;
        } else if (take.isOnError()) {
          completed = true;
          throw new RuntimeException(take.getError());
        } else {
          completed = true;
        }
      }
    }
  }

  public static final class FlowableIterator<T> implements Iterator<T> {
    private Notification<T> next = null;
    private LinkedBlockingQueue<Notification<T>> queue = new LinkedBlockingQueue<>(1);
    private boolean completed = false;
    private Subscription subscription;

    public FlowableIterator(Flowable<T> source) {
      source
          .materialize()
          .subscribe(
              e -> queue.put(e),
              e -> completed = true,
              () -> completed = true,
              s -> this.subscription = s
          );
    }

    @Override
    public boolean hasNext() {
      calcNext();
      return !completed;
    }

    @Override
    public T next() {
      calcNext();
      if (next == null) {
        throw new NoSuchElementException();
      }
      T t = next.getValue();
      next = null;
      return t;
    }

    private void calcNext() {
      if (completed) {
        return;
      }
      if (next == null) {
        subscription.request(1);
        Notification<T> take = ExceptionUtil.uncheck(() -> queue.take());
        if (take.isOnNext()) {
          next = take;
        } else if (take.isOnError()) {
          completed = true;
          throw new RuntimeException(take.getError());
        } else {
          completed = true;
        }
      }
    }
  }
}
