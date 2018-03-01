package xdean.jex.util;

import static xdean.jex.util.function.FunctionAdapter.supplierToRunnable;

import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import com.google.common.base.Stopwatch;

import xdean.codecov.CodecovIgnore;
import xdean.jex.util.cache.CacheUtil;

@CodecovIgnore
public class TimingUtil {
  public static void timeThen(Runnable r, Consumer<Long> then) {
    Stopwatch s = getShareStopwatch();
    s.reset();
    s.start();
    r.run();
    s.stop();
    if (then != null) {
      then.accept(s.elapsed(TimeUnit.MILLISECONDS));
    }
  }

  public static void timeThenPrint(Runnable r, String format) {
    timeThen(r, l -> System.out.printf(format, l));
  }

  public static <T> T timeThen(Supplier<T> s, Consumer<Long> then) {
    return supplierToRunnable(s, t -> timeThen(t, then));
  }

  public static <T> T timeThenPrint(Supplier<T> r, String format) {
    return timeThen(r, l -> System.out.printf(format, l));
  }

  /**
   * @param uniqueKey unique key
   * @param r the task
   * @param then (this time, total time) -&#62; {...}
   */
  public static void seriesTimeThen(Object uniqueKey, Runnable r, BiConsumer<Long, Long> then) {
    Stopwatch total = CacheUtil.cache(TimingUtil.class, uniqueKey, () -> Stopwatch.createUnstarted());
    Stopwatch temp = getShareStopwatch();
    temp.reset();
    temp.start();
    total.start();
    r.run();
    temp.stop();
    total.stop();
    if (then != null) {
      then.accept(temp.elapsed(TimeUnit.MILLISECONDS), total.elapsed(TimeUnit.MILLISECONDS));
    }
  }

  public static void seriesTimeThen(Runnable r, BiConsumer<Long, Long> then) {
    seriesTimeThen(r, r, then);
  }

  public static <T> T seriesTimeThen(Object uniqueKey, Supplier<T> s, BiConsumer<Long, Long> then) {
    return supplierToRunnable(s, t -> seriesTimeThen(uniqueKey, t, then));
  }

  public static <T> T seriesTimeThen(Supplier<T> s, BiConsumer<Long, Long> then) {
    return seriesTimeThen(s, s, then);
  }

  private static Stopwatch getShareStopwatch() {
    Stopwatch share = CacheUtil.cache(TimingUtil.class, Thread.currentThread(), () -> Stopwatch.createUnstarted());
    return share.isRunning() ? Stopwatch.createUnstarted() : share;
  }
}
