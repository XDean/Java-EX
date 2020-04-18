package cn.xdean.jex.lang.collection;

import cn.xdean.jex.lang.ExceptionUtil;
import cn.xdean.jex.lang.function.type.ActionE0;
import org.junit.Assert;
import org.junit.Test;

import java.util.NoSuchElementException;

import static org.junit.Assert.*;

public class EitherTest {

  @Test
  public void testLeft() throws Exception {
    Either<Integer, Integer> left = Either.left(1);
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
    Assert.assertEquals(2, left.flatMapLeft(i -> Either.left(2)).getLeft().intValue());
    assertEquals(1, left.flatMapRight(i -> Either.left(2)).getLeft().intValue());
    Assert.assertEquals(2, left.flatMap(i -> Either.left(2), i -> fail()).getLeft().intValue());
    assertEquals(1, left.unify(i -> 1, i -> 2).intValue());
    Assert.assertEquals(left, Either.left(1));
    assertEquals("left(1)", left.toString());
  }

  @Test
  public void testRight() throws Exception {
    Either<Integer, Integer> right = Either.right(1);
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
    Assert.assertEquals(2, right.flatMapRight(i -> Either.right(2)).getRight().intValue());
    assertEquals(1, right.flatMapLeft(i -> Either.right(2)).getRight().intValue());
    Assert.assertEquals(2, right.flatMap(i -> fail(), i -> Either.right(2)).getRight().intValue());
    assertEquals(2, right.unify(i -> 1, i -> 2).intValue());
    Assert.assertEquals(right, Either.right(1));
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
    ExceptionUtil.uncheck(() -> a.call());
    assertTrue("Don't reach there.", here == 0);
  }

  private static void assertNotReach(ActionE0<?> a) {
    ExceptionUtil.uncheck(() -> a.call());
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
