package xdean.jex.extra.rx;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import rx.Observable;
import rx.Scheduler;
import rx.internal.util.RxThreadFactory;
import rx.schedulers.Schedulers;
import xdean.jex.extra.Pair;
import xdean.jex.util.lang.FinalizeSupport;

public class RxUtil {
  public static Scheduler fixedSizeScheduler(int size) {
    ExecutorService pool = Executors.newFixedThreadPool(size, new RxThreadFactory("FixedSizeScheduler-"));
    Scheduler scheduler = Schedulers.from(pool);
    FinalizeSupport.finalize(scheduler, () -> pool.shutdown());
    return scheduler;
  }

  /**
   * 
   * @param from
   *          include
   * @param to
   *          include
   * @param step
   * @return
   */
  public static Observable<Integer> range(int from, int to, int step) {
    return Observable.range(0, 1 + (to - from) / step + ((to - from) % step == 0 ? 0 : 1)).map(i -> from + i * step);
  }

  public static Observable<Double> range(double from, double to, double step) {
    return Observable.range(0, (int) ((to - from) / step + ((to - from) % step == 0 ? 0 : 1))).map(i -> from + i * step);
  }

  public static <A, B> Observable<Pair<A, B>> cross(Observable<A> oa, Observable<B> ob) {
    return oa.flatMap(a -> ob.map(b -> Pair.of(a, b)));
  }
}
