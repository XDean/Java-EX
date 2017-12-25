package xdean.jex.util.lang;

import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import io.reactivex.Scheduler;
import io.reactivex.schedulers.Schedulers;

/**
 * Provide a convenient way to do something on the moment when an object be collected by gc.
 *
 * @author Dean Xu (XDean@github.com)
 *
 */
public class FinalizeSupport {

  private static final Map<Reference<?>, Runnable> FINALIZE_TASK_MAP = new ConcurrentHashMap<>();
  private static final ReferenceQueue<Object> QUEUE = new ReferenceQueue<>();

  static {
    ThreadGroup tg = Thread.currentThread().getThreadGroup();
    for (ThreadGroup tgn = tg; tgn != null; tg = tgn, tgn = tg.getParent()) {
    }
    Thread handler = new FinalizeHandler(tg, "FinalizeSupport");
    handler.setPriority(Thread.MAX_PRIORITY - 2);
    handler.setDaemon(true);
    handler.start();
  }

  public static void finalize(Object o, Runnable r) {
    finalize(o, r, Schedulers.io());
  }

  public static void finalize(Object o, Runnable r, Scheduler s) {
    FINALIZE_TASK_MAP.put(new PhantomReference<>(o, QUEUE), () -> s.createWorker().schedule(r));
  }

  private static class FinalizeHandler extends Thread {
    public FinalizeHandler(ThreadGroup tg, String name) {
      super(tg, name);
    }

    @Override
    public void run() {
      while (!Thread.interrupted()) {
        try {
          Reference<? extends Object> ref = QUEUE.remove();
          if (ref != null) {
            Optional.ofNullable(FINALIZE_TASK_MAP.remove(ref)).ifPresent(r -> r.run());
          }
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
      }
    }
  }
}