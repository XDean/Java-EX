package xdean.jex.extra.rx2;

import static xdean.jex.util.lang.ExceptionUtil.uncheck;
import io.reactivex.Flowable;
import io.reactivex.Notification;
import io.reactivex.Observable;
import io.reactivex.Scheduler;
import io.reactivex.annotations.SchedulerSupport;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;
import io.reactivex.schedulers.Schedulers;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.LinkedBlockingQueue;

import org.reactivestreams.Subscription;

/**
 * Convert RxStream to Iterator.
 *
 * @author XDean, Michael@stackoverflow
 *
 */
public class RxIterator {
  public static <T> Function<Flowable<T>, Iterator<T>> flowableIterator(Scheduler scheduler) {
    return o -> toIterator(o, scheduler);
  }

  public static <T> Function<Flowable<T>, Iterator<T>> flowableIterator() {
    return o -> toIterator(o);
  }

  public static <T> Iterator<T> toIterator(Flowable<T> ob, Scheduler scheduler) {
    return new FlowableIterator<>(ob, scheduler);
  }

  @SchedulerSupport(SchedulerSupport.IO)
  public static <T> Iterator<T> toIterator(Flowable<T> ob) {
    return toIterator(ob, Schedulers.io());
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

  public static final class ObservableIterator<T> implements Iterator<T>, AutoCloseable {
    private LinkedBlockingQueue<Notification<T>> queue = new LinkedBlockingQueue<>();
    private Optional<Notification<T>> next = Optional.empty();
    private boolean completed = false;
    private Disposable disposable;

    public ObservableIterator(Observable<T> source, Scheduler scheduler) {
      disposable = source
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
      if (next.isPresent() == false) {
        throw new NoSuchElementException();
      }
      T t = next.get().getValue();
      next = Optional.empty();
      return t;
    }

    private void calcNext() {
      if (completed) {
        return;
      }
      if (next.isPresent() == false) {
        Notification<T> take = uncheck(queue::take);
        if (take.isOnNext()) {
          next = Optional.of(take);
        } else if (take.isOnError()) {
          completed = true;
          throw new RuntimeException(take.getError());
        } else {
          completed = true;
        }
      }
    }

    @Override
    public void close() throws Exception {
      disposable.dispose();
      completed = true;
      next = null;
    }
  }

  public static final class FlowableIterator<T> implements Iterator<T>, AutoCloseable {
    private LinkedBlockingQueue<Notification<T>> queue = new LinkedBlockingQueue<>(1);
    private Optional<Notification<T>> next = Optional.empty();
    private boolean completed = false;
    private Subscription subscription;

    public FlowableIterator(Flowable<T> source, Scheduler scheduler) {
      source
          .materialize()
          .subscribeOn(scheduler)
          .subscribe(
              queue::put,
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
      if (next.isPresent() == false) {
        throw new NoSuchElementException();
      }
      T t = next.get().getValue();
      next = Optional.empty();
      return t;
    }

    private void calcNext() {
      if (completed) {
        return;
      }
      if (next.isPresent() == false) {
        subscription.request(1);
        Notification<T> take = uncheck(queue::take);
        if (take.isOnNext()) {
          next = Optional.of(take);
        } else if (take.isOnError()) {
          completed = true;
          throw new RuntimeException(take.getError());
        } else {
          completed = true;
        }
      }
    }

    @Override
    public void close() throws Exception {
      subscription.cancel();
      completed = true;
      next = null;
    }
  }
}
