package cn.xdean.jex.extra;

import static cn.xdean.jex.util.lang.ExceptionUtil.throwAsUncheck;
import static cn.xdean.jex.util.lang.ExceptionUtil.throwIt;
import static org.junit.Assert.*;
import static cn.xdean.jex.util.function.Predicates.is;

import java.util.NoSuchElementException;

import cn.xdean.jex.extra.function.ActionE0;
import cn.xdean.jex.extra.tryto.Try;
import org.junit.Assert;
import org.junit.Test;

public class TryTest {
  int hitCount;

  @Test
  public void testFail() throws Exception {
    assertTrue(Try.ofFailure(new IllegalAccessException()).failed().get() instanceof IllegalAccessException);
    assertTrue(Try.of(1).failed().failed().get() instanceof UnsupportedOperationException);
  }

  @Test
  public void testTo() throws Exception {
    assertTrue(Try.to((ActionE0<Exception>) (() -> hitCount++)).isSuccess());
    assertEquals(1, hitCount);
    assertTrue(Try.to(() -> hitCount++).isSuccess());
    assertEquals(2, hitCount);
    assertTrue(Try.to(() -> throwIt(new Exception()), () -> hitCount += 10).isFailure());
    assertEquals(12, hitCount);
    assertTrue(Try.to(() -> throwIt(new Exception()), () -> throwIt(new Exception())).isFailure());
    assertEquals(12, hitCount);
  }

  @Test
  public void testGet() throws Exception {
    Assert.assertEquals(1, Try.of(1).get().intValue());
    Assert.assertEquals(1, Try.of(1).getOrElse(2).intValue());
    Assert.assertEquals(1, Try.of(1).getOrElse(() -> 2).intValue());
    assertEquals(2, error(1).getOrElse(2).intValue());
    assertEquals(3, error(1).getOrElse(() -> 3).intValue());
    assertEquals(2, error(1).orElse(Try.of(2)).get().intValue());
    assertEquals(3, error(1).orElse(() -> Try.of(3)).get().intValue());
    assertTrue(error(1).orElse(() -> throwIt(new RuntimeException())).isFailure());
    assertTrue(Try.of(1).toOptional().isPresent());
    assertFalse(error(1).toOptional().isPresent());
  }

  @Test
  public void testRecover() throws Exception {
    Assert.assertEquals(1, Try.of(1).recover(e -> 2).get().intValue());
    assertEquals(2, error(1).recover(e -> 2).get().intValue());
    Assert.assertEquals(1, Try.of(1).recoverWith(e -> error(2)).get().intValue());
    assertTrue(error(1).recoverWith(e -> error(2)).isFailure());
    assertTrue(error(1).recoverWith(e -> throwIt(new RuntimeException())).isFailure());
    assertEquals(2, error(1).recoverWith(e -> Try.of(2)).get().intValue());
  }

  @Test
  public void testTransform() throws Exception {
    Assert.assertEquals(1, Try.of(1).transform(i -> Try.of(i), e -> Try.of(100)).get().intValue());
    Assert.assertEquals(1, Try.of(1).transform(i -> Try.of(i), e -> throwIt(new RuntimeException())).get().intValue());
    assertTrue(Try.of(1).transform(i -> throwIt(new RuntimeException()), e -> Try.of(100)).isFailure());
    Assert.assertEquals(100, Try.ofFailure(new Exception()).transform(i -> Try.of(i), e -> Try.of(100)).get());
    assertTrue(Try.ofFailure(new Exception()).transform(i -> Try.of(i), e -> throwIt(new RuntimeException()))
        .isFailure());
    Assert.assertEquals(100, Try.ofFailure(new Exception()).transform(i -> throwIt(new RuntimeException()), e -> Try.of(100))
        .get().intValue());
  }

  @Test
  public void testFilter() throws Exception {
    Assert.assertEquals(1, Try.of(1).filter(is(1)).get().intValue());
    assertTrue(Try.of(1).filter(is(2)).failed().get() instanceof NoSuchElementException);
    assertTrue(Try.of(1).filter(i -> throwIt(new RuntimeException())).isFailure());
    assertTrue(error(1).filter(i -> throwIt(new RuntimeException())).isFailure());
  }

  @Test
  public void testOnException() throws Exception {
    Try.of(1).onException(e -> hitCount++);
    assertEquals(0, hitCount);
    error(1).onException(e -> hitCount++);
    assertEquals(1, hitCount);
  }

  @Test
  public void testFlatMap() throws Exception {
    Assert.assertEquals(2, Try.of(1).flatMap(i -> Try.of(i * 2)).get().intValue());
    assertTrue(error(1).flatMap(i -> Try.of(i * 2)).isFailure());
  }

  @Test(expected = RuntimeException.class)
  public void testGetError() throws Exception {
    error(1).get();
  }

  private Try<Integer> error(int i) {
    return Try.of(i).map(ii -> throwAsUncheck(new Exception()));
  }
}
