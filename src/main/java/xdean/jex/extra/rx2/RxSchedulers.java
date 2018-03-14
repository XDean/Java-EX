package xdean.jex.extra.rx2;

import static xdean.jex.util.log.LogUtil.debug;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory;
import java.util.concurrent.ForkJoinWorkerThread;
import java.util.concurrent.ThreadFactory;

import io.reactivex.Scheduler;
import io.reactivex.internal.schedulers.RxThreadFactory;
import io.reactivex.schedulers.Schedulers;
import xdean.jex.extra.LazyValue;
import xdean.jex.util.lang.FinalizeSupport;

public class RxSchedulers {
  /**
   * @see https://github.com/ReactiveX/RxJava/issues/5822
   */
  private static final LazyValue<Scheduler> NEW_IO = LazyValue.create(
      () -> Schedulers.from(Executors.newCachedThreadPool(new RxThreadFactory("RxSchedulers.newIOScheduler"))));

  public static Scheduler newIO() {
    return NEW_IO.get();
  }

  public static Scheduler fixedSize(int size) {
    return autoClose(Executors.newFixedThreadPool(size, new ThreadFactory() {
      int i = 0;

      @Override
      public Thread newThread(Runnable r) {
        Thread t = new Thread(r);
        t.setName("FixedSizeScheduler(size=" + size + ")-" + (++i));
        t.setPriority(Thread.NORM_PRIORITY);
        t.setDaemon(true);
        return t;
      }
    }));
  }

  public static Scheduler forkJoin(int size) {
    class FJWT extends ForkJoinWorkerThread {
      protected FJWT(ForkJoinPool pool) {
        super(pool);
      }
    }
    return autoClose(new ForkJoinPool(size, new ForkJoinWorkerThreadFactory() {
      int i = 0;

      @Override
      public ForkJoinWorkerThread newThread(ForkJoinPool pool) {
        ForkJoinWorkerThread t = new FJWT(pool);
        t.setName("ForkJoinScheduler(parallelism=" + size + ")-" + (++i));
        t.setPriority(Thread.NORM_PRIORITY);
        t.setDaemon(true);
        return t;
      }
    }, null, true));
  }

  public static Scheduler autoClose(ExecutorService pool) {
    Scheduler scheduler = Schedulers.from(pool);
    FinalizeSupport.finalize(scheduler, () -> {
      debug().log("Shutdown the scheduler from: " + pool);
      pool.shutdown();
    });
    return scheduler;
  }
}
