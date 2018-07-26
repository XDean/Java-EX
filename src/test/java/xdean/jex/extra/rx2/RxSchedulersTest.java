package xdean.jex.extra.rx2;

import static org.junit.Assert.*;
import io.reactivex.Observable;
import io.reactivex.Scheduler;

import java.lang.Thread.State;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import xdean.jex.extra.collection.Wrapper;
import xdean.jex.util.lang.FinalizeSupport;

public class RxSchedulersTest {
  int hitCount;

  @Test
  public void testGC() throws Exception {
    CountDownLatch done = new CountDownLatch(1);
    Scheduler scheduler = RxSchedulers.fixedSize(1);
    FinalizeSupport.finalize(scheduler, done::countDown);
    Wrapper<Thread> worker = Wrapper.empty();
    Observable.just(1)
        .subscribeOn(scheduler)
        .doOnNext(i -> worker.set(Thread.currentThread()))
        .blockingSubscribe();
    scheduler = null;
    for (int i = 0; i < 10; i++) {
      Thread.sleep(10);
      System.gc();
    }
    assertTrue(done.await(1000, TimeUnit.MILLISECONDS));
    assertNotNull(worker.get());
    for (int i = 0; i < 20; i++) {
      System.gc();
      Thread.sleep(5);
      if (worker.get().getState() == State.TERMINATED) {
        return;
      }
    }
    assertEquals(State.TERMINATED, worker.get().getState());
  }

  @Test
  public void testForkJoin() throws Exception {
    Scheduler scheduler = RxSchedulers.forkJoin(1);
    scheduler.createWorker().schedule(() -> hitCount++);
    scheduler.shutdown();
  }
}
