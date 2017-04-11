package xdean.jex.extra.rx.op;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.LockSupport;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.experimental.FieldDefaults;
import rx.Observable.Operator;
import rx.Scheduler;
import rx.Subscriber;

/**
 * An operator to do tasks on specified scheduler and wait for all tasks completed.
 * 
 * @author XDean
 *
 */
@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
@AllArgsConstructor
public class ParallelOperator<T> implements Operator<T, T> {

  Scheduler scheduler;

  @Override
  public Subscriber<? super T> call(Subscriber<? super T> s) {
    ParallelSubscriber ts = new ParallelSubscriber(s);
    s.add(ts);
    return ts;
  }

  private class ParallelSubscriber extends Subscriber<T> {

    private final Subscriber<? super T> actual;
    private final AtomicInteger endLeft = new AtomicInteger(0);
    // private final AtomicInteger startLeft = new AtomicInteger(0);
    private volatile Thread completeThread;

    // private volatile Thread nextThread;

    public ParallelSubscriber(Subscriber<? super T> actual) {
      this.actual = actual;
    }

    @Override
    public void onNext(T next) {
      endLeft.incrementAndGet();
      scheduler.createWorker().schedule(() -> {
        actual.onNext(next);
        if (endLeft.decrementAndGet() == 0 && completeThread != null) {
          LockSupport.unpark(completeThread);
          completeThread = null;
        }
      });
    }

    @Override
    public void onCompleted() {
      completeThread = Thread.currentThread();
      LockSupport.park();
      actual.onCompleted();
    }

    @Override
    public void onError(Throwable e) {
      actual.onError(e);
    }
  }
}
