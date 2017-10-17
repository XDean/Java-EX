package xdean.jex.util.task;

import static org.junit.Assert.*;
import static xdean.jex.util.lang.ExceptionUtil.throwIt;
import static xdean.jex.util.task.TaskUtil.*;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import xdean.jex.util.function.EmptyFunction;

public class TaskUtilTest {
  int hitCount;

  @Test
  public void testAsync() throws Exception {
    Thread out = Thread.currentThread();
    CountDownLatch done = new CountDownLatch(1);
    async(() -> {
      assertNotEquals(Thread.currentThread(), out);
      done.countDown();
    });
    done.await(10, TimeUnit.MILLISECONDS);
  }

  @Test
  public void testDoAll() throws Exception {
    TaskUtil.todoAll(
        () -> hitCount += 1,
        () -> hitCount += 10,
        () -> hitCount += 100
        );
    assertEquals(111, hitCount);
  }

  @Test
  public void testFirstSuccess() throws Exception {
    assertEquals(1, TaskUtil.firstSuccess(
        () -> throwIt(new Exception()),
        () -> 1
        ).intValue());
  }

  @Test(expected = IllegalStateException.class)
  public void testFirstSuccessFail() throws Exception {
    TaskUtil.firstSuccess(
        () -> throwIt(new Exception()),
        () -> throwIt(new Exception())
        );
  }

  @Test
  public void testFirstFail() throws Exception {
    assertTrue(TaskUtil.firstFail(
        () -> hitCount += 1,
        () -> throwIt(new Exception()),
        () -> hitCount += 10
        ).isPresent());
    assertEquals(1, hitCount);
    assertFalse(TaskUtil.firstFail(() -> hitCount += 1).isPresent());
  }

  @Test
  public void testFirstNonNull() throws Exception {
    assertEquals(1, TaskUtil.firstNonNull(
        () -> null,
        () -> throwIt(new Exception()),
        () -> 1
        ).get().intValue());
    assertFalse(TaskUtil.firstNonNull().isPresent());
  }

  @Test
  public void testAndFinal() throws Exception {
    andFinal(EmptyFunction.supplier(), o -> hitCount++);
    assertEquals(1, hitCount);
    andFinal(EmptyFunction.runnable(), () -> hitCount++);
    assertEquals(2, hitCount);
  }
}
