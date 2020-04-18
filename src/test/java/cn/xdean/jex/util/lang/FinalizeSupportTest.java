package cn.xdean.jex.util.lang;

import static org.junit.Assert.*;

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Test;

import cn.xdean.jex.util.task.If;

public class FinalizeSupportTest {
  @Test
  public void test() throws Exception {
    AtomicBoolean called = new AtomicBoolean(false);
    FinalizeSupport.finalize(new Object(),
        () -> If.that(called.compareAndSet(false, true)).ordo(() -> fail("Finalize runnable called more than once!")));
    System.gc();
    Thread.sleep(10);
    System.gc();
    Thread.sleep(10);
    assertTrue(called.get());
  }
}
