package xdean.jex.util.lang;

import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import lombok.experimental.UtilityClass;
import xdean.jex.util.reflect.ReflectUtil;

@UtilityClass
public class FinalizeSupport {

  Map<Reference<?>, Runnable> map = new ConcurrentHashMap<>();
  ReferenceQueue<Object> queue = new ReferenceQueue<>();
  Object lock;

  static {
    try {
      lock = ReflectUtil.getField(ReferenceQueue.class, queue, "lock");
    } catch (NoSuchFieldException e) {
      throw new Error("Can't find ReferenceQueue's lock, Check code and java version");
    }

    ThreadGroup tg = Thread.currentThread().getThreadGroup();
    for (ThreadGroup tgn = tg; tgn != null; tg = tgn, tgn = tg.getParent()) {
      ;
    }
    Thread handler = new FinalizeHandler(tg, "Finalize Support");
    handler.setPriority(Thread.MAX_PRIORITY);
    handler.setDaemon(true);
    handler.start();
  }

  public void finalize(Object o, Runnable r) {
    map.put(new PhantomReference<>(o, queue), r);
  }

  private static class FinalizeHandler extends Thread {
    public FinalizeHandler(ThreadGroup tg, String name) {
      super(tg, name);
    }

    @Override
    public void run() {
      synchronized (lock) {
        while (!Thread.interrupted()) {
          Reference<? extends Object> ref = queue.poll();
          if (ref == null) {
            try {
              lock.wait();
            } catch (InterruptedException e) {
              Thread.currentThread().interrupt();
            }
          } else {
            Optional.ofNullable(map.remove(ref)).ifPresent(r -> r.run());
          }
        }
      }
    }
  }
}
