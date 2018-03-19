package xdean.jex.extra;

import static xdean.jex.util.lang.ExceptionUtil.*;
import static xdean.jex.extra.Either.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.NoSuchElementException;

import org.junit.Assert;
import org.junit.Test;

import xdean.jex.extra.function.ActionE0;

public class EitherTest {

  @Test
  public void testLeft() throws Exception {
    Either<Integer, Integer> left = left(1);
    assertTrue(left.isLeft());
    assertFalse(left.isRight());
    assertEquals(1, left.getLeft().intValue());
    assertThrow(NoSuchElementException.class, () -> left.getRight());
    assertEquals(1, left.toLeft(i -> 2).intValue());
    assertEquals(2, left.toRight(i -> 2).intValue());
    assertTrue(left.asLeft().isPresent());
    assertFalse(left.asRight().isPresent());
    assertReach(() -> left.ifLeft(l -> here()));
    assertNotReach(() -> left.ifRight(l -> here()));
    assertReach(() -> left.exec(i -> here(), i -> fail()));
    assertEquals(2, left.mapLeft(i -> 2).getLeft().intValue());
    assertEquals(1, left.mapRight(i -> 2).getLeft().intValue());
    assertEquals(2, left.map(i -> 2, i -> fail()).getLeft().intValue());
    assertEquals(2, left.flatMapLeft(i -> left(2)).getLeft().intValue());
    assertEquals(1, left.flatMapRight(i -> left(2)).getLeft().intValue());
    assertEquals(2, left.flatMap(i -> left(2), i -> fail()).getLeft().intValue());
    assertEquals(1, left.unify(i -> 1, i -> 2).intValue());
    assertEquals(left, left(1));
    assertEquals("left(1)", left.toString());
  }

  @Test
  public void testRight() throws Exception {
    Either<Integer, Integer> right = right(1);
    assertTrue(right.isRight());
    assertFalse(right.isLeft());
    assertEquals(1, right.getRight().intValue());
    assertThrow(NoSuchElementException.class, () -> right.getLeft());
    assertEquals(1, right.toRight(i -> 2).intValue());
    assertEquals(2, right.toLeft(i -> 2).intValue());
    assertTrue(right.asRight().isPresent());
    assertFalse(right.asLeft().isPresent());
    assertReach(() -> right.ifRight(l -> here()));
    assertNotReach(() -> right.ifLeft(l -> here()));
    assertReach(() -> right.exec(i -> fail(), i -> here()));
    assertEquals(2, right.mapRight(i -> 2).getRight().intValue());
    assertEquals(1, right.mapLeft(i -> 2).getRight().intValue());
    assertEquals(2, right.map(i -> fail(), i -> 2).getRight().intValue());
    assertEquals(2, right.flatMapRight(i -> right(2)).getRight().intValue());
    assertEquals(1, right.flatMapLeft(i -> right(2)).getRight().intValue());
    assertEquals(2, right.flatMap(i -> fail(), i -> right(2)).getRight().intValue());
    assertEquals(2, right.unify(i -> 1, i -> 2).intValue());
    assertEquals(right, right(1));
    assertEquals("right(1)", right.toString());
  }

  private static void assertThrow(Class<? extends Exception> ex, ActionE0<?> a) {
    try {
      a.call();
    } catch (Exception e) {
      if (!ex.isInstance(e)) {
        fail();
      }
    }
  }

  private static transient int here = 0;

  private static void assertReach(ActionE0<?> a) {
    here++;
    uncheck(() -> a.call());
    assertTrue("Don't reach there.", here == 0);
  }

  private static void assertNotReach(ActionE0<?> a) {
    uncheck(() -> a.call());
    assertTrue("Indeed reach there.", here == 0);
  }

  private static void here() {
    here--;
  }

  private static <R> R fail() {
    Assert.fail();
    return null;
  }
}
