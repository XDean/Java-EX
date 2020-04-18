package cn.xdean.jex.lang.unsafe;

import cn.xdean.jex.lang.function.If;
import org.junit.Test;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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
