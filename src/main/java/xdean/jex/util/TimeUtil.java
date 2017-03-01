package xdean.jex.util;

import static xdean.jex.util.function.FunctionAdapter.supplierToRunnable;

import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import lombok.experimental.UtilityClass;
import xdean.jex.util.cache.CacheUtil;

import com.google.common.base.Stopwatch;

@UtilityClass
public class TimeUtil {
  public void timeThen(Runnable r, Consumer<Long> then) {
    Stopwatch s = getShareStopwatch();
    s.reset();
    s.start();
    r.run();
    s.stop();
    then.accept(s.elapsed(TimeUnit.MILLISECONDS));
  }

  public void timeThenPrint(Runnable r, String format) {
    timeThen(r, l -> System.out.printf(format, l));
  }

  public <T> T timeThen(Supplier<T> s, Consumer<Long> then) {
    return supplierToRunnable(s, t -> timeThen(t, then));
  }

  public <T> T timeThenPrint(Supplier<T> r, String format) {
    return timeThen(r, l -> System.out.printf(format, l));
  }

  /**
   * 
   * @param uniqueKey
   * @param r
   * @param then
   *          (this time, total time) -> {...}
   */
  public void seriesTimeThen(Object uniqueKey, Runnable r, BiConsumer<Long, Long> then) {
    Stopwatch total = CacheUtil.cache(TimeUtil.class, uniqueKey, () -> Stopwatch.createUnstarted());
    Stopwatch temp = getShareStopwatch();
    temp.reset();
    temp.start();
    total.start();
    r.run();
    temp.stop();
    total.stop();
    then.accept(temp.elapsed(TimeUnit.MILLISECONDS), total.elapsed(TimeUnit.MILLISECONDS));
  }

  public void seriesTimeThen(Runnable r, BiConsumer<Long, Long> then) {
    seriesTimeThen(r, r, then);
  }

  public <T> T seriesTimeThen(Object uniqueKey, Supplier<T> s, BiConsumer<Long, Long> then) {
    return supplierToRunnable(s, t -> seriesTimeThen(uniqueKey, t, then));
  }

  public <T> T seriesTimeThen(Supplier<T> s, BiConsumer<Long, Long> then) {
    return seriesTimeThen(s, s, then);
  }

  private Stopwatch getShareStopwatch() {
    Stopwatch share = CacheUtil.cache(TimeUtil.class, Thread.currentThread(), () -> Stopwatch.createUnstarted());
    return share.isRunning() ? Stopwatch.createUnstarted() : share;
  }
}
