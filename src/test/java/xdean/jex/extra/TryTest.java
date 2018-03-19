package xdean.jex.extra;

import static org.junit.Assert.*;
import static xdean.jex.extra.tryto.Try.*;
import static xdean.jex.util.function.Predicates.is;
import static xdean.jex.util.lang.ExceptionUtil.*;

import java.util.NoSuchElementException;

import org.junit.Test;

import xdean.jex.extra.function.ActionE0;
import xdean.jex.extra.tryto.Try;

public class TryTest {
  int hitCount;

  @Test
  public void testFail() throws Exception {
    assertTrue(ofFailure(new IllegalAccessException()).failed().get() instanceof IllegalAccessException);
    assertTrue(of(1).failed().failed().get() instanceof UnsupportedOperationException);
  }

  @Test
  public void testTo() throws Exception {
    assertTrue(to((ActionE0<Exception>) (() -> hitCount++)).isSuccess());
    assertEquals(1, hitCount);
    assertTrue(to(() -> hitCount++).isSuccess());
    assertEquals(2, hitCount);
    assertTrue(to(() -> throwIt(new Exception()), () -> hitCount += 10).isFailure());
    assertEquals(12, hitCount);
    assertTrue(to(() -> throwIt(new Exception()), () -> throwIt(new Exception())).isFailure());
    assertEquals(12, hitCount);
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
    of(1).onException(e -> hitCount++);
    assertEquals(0, hitCount);
    error(1).onException(e -> hitCount++);
    assertEquals(1, hitCount);
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
