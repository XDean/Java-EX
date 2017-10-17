package xdean.jex.util.task;

import static org.junit.Assert.*;
import static xdean.jex.util.function.Predicates.is;
import static xdean.jex.util.lang.ExceptionUtil.*;
import static xdean.jex.util.task.tryto.Try.*;

import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.util.task.tryto.Try;

public class TryTest {
  AtomicInteger count = new AtomicInteger(0);

  @Test
  public void testFail() throws Exception {
    assertTrue(ofFailure(new IllegalAccessException()).failed().get() instanceof IllegalAccessException);
    assertTrue(of(1).failed().failed().get() instanceof UnsupportedOperationException);
  }

  @Test
  public void testTo() throws Exception {
    assertTrue(to((RunnableThrow<Exception>) (() -> count.incrementAndGet())).isSuccess());
    assertEquals(1, count.get());
    assertTrue(to(() -> count.incrementAndGet()).isSuccess());
    assertEquals(2, count.get());
    assertTrue(to(() -> throwIt(new Exception()), () -> count.addAndGet(10)).isFailure());
    assertEquals(12, count.get());
    assertTrue(to(() -> throwIt(new Exception()), () -> throwIt(new Exception())).isFailure());
    assertEquals(12, count.get());
  }

  @Test
  public void testGet() throws Exception {
    assertEquals(1, of(1).get().intValue());
    assertEquals(1, of(1).getOrElse(2).intValue());
    assertEquals(1, of(1).getOrElse(() -> 2).intValue());
    assertEquals(2, error(1).getOrElse(2).intValue());
    assertEquals(3, error(1).getOrElse(() -> 3).intValue());
    assertEquals(2, error(1).orElse(of(2)).get().intValue());
    assertEquals(3, error(1).orElse(() -> of(3)).get().intValue());
    assertTrue(error(1).orElse(() -> throwIt(new RuntimeException())).isFailure());
    assertTrue(of(1).toOptional().isPresent());
    assertFalse(error(1).toOptional().isPresent());
  }

  @Test
  public void testRecover() throws Exception {
    assertEquals(1, of(1).recover(e -> 2).get().intValue());
    assertEquals(2, error(1).recover(e -> 2).get().intValue());
    assertEquals(1, of(1).recoverWith(e -> error(2)).get().intValue());
    assertTrue(error(1).recoverWith(e -> error(2)).isFailure());
    assertTrue(error(1).recoverWith(e -> throwIt(new RuntimeException())).isFailure());
    assertEquals(2, error(1).recoverWith(e -> of(2)).get().intValue());
  }

  @Test
  public void testTransform() throws Exception {
    assertEquals(1, of(1).transform(i -> of(i), e -> of(100)).get().intValue());
    assertEquals(1, of(1).transform(i -> of(i), e -> throwIt(new RuntimeException())).get().intValue());
    assertTrue(of(1).transform(i -> throwIt(new RuntimeException()), e -> of(100)).isFailure());
    assertEquals(100, ofFailure(new Exception()).transform(i -> of(i), e -> of(100)).get());
    assertTrue(ofFailure(new Exception()).transform(i -> of(i), e -> throwIt(new RuntimeException()))
        .isFailure());
    assertEquals(100, ofFailure(new Exception()).transform(i -> throwIt(new RuntimeException()), e -> of(100))
        .get().intValue());
  }

  @Test
  public void testFilter() throws Exception {
    assertEquals(1, of(1).filter(is(1)).get().intValue());
    assertTrue(of(1).filter(is(2)).failed().get() instanceof NoSuchElementException);
    assertTrue(of(1).filter(i -> throwIt(new RuntimeException())).isFailure());
    assertTrue(error(1).filter(i -> throwIt(new RuntimeException())).isFailure());
  }

  @Test
  public void testOnException() throws Exception {
    of(1).onException(e -> count.incrementAndGet());
    assertEquals(0, count.get());
    error(1).onException(e -> count.incrementAndGet());
    assertEquals(1, count.get());
  }

  @Test
  public void testFlatMap() throws Exception {
    assertEquals(2, of(1).flatMap(i -> of(i * 2)).get().intValue());
    assertTrue(error(1).flatMap(i -> of(i * 2)).isFailure());
  }

  @Test(expected = RuntimeException.class)
  public void testGetError() throws Exception {
    error(1).get();
  }

  private Try<Integer> error(int i) {
    return of(i).map(ii -> throwAsUncheck(new Exception()));
  }
}
