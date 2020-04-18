package cn.xdean.jex.lang.math;

import org.junit.Test;

import static org.junit.Assert.*;

public class MathUtilTest {
  @Test
  public void testSquareSum() throws Exception {
    assertEquals(1, MathUtil.squareSum(1));
    assertEquals(14, MathUtil.squareSum(1, 2, 3));
    assertEquals(30, MathUtil.squareSum(1, 2, 3, 4));
    assertEquals(1d, MathUtil.squareSum(1d), 0d);
    assertEquals(14d, MathUtil.squareSum(1d, 2d, 3d), 0d);
    assertEquals(30d, MathUtil.squareSum(1d, 2d, 3d, 4d), 0d);
  }

  @Test
  public void testInRange() throws Exception {
    assertTrue(MathUtil.inRange(5, 1, 10));
    assertTrue(MathUtil.inRange(1, 1, 10));
    assertTrue(MathUtil.inRange(10, 1, 10));
    assertFalse(MathUtil.inRange(Integer.MIN_VALUE, 1, 10));
    assertFalse(MathUtil.inRange(Integer.MAX_VALUE, 1, 10));
    assertTrue(MathUtil.inRange(5d, 1d, 10d));
    assertTrue(MathUtil.inRange(1d, 1d, 10d));
    assertTrue(MathUtil.inRange(10d, 1d, 10d));
    assertFalse(MathUtil.inRange(Integer.MIN_VALUE, 1d, 10d));
    assertFalse(MathUtil.inRange(Integer.MAX_VALUE, 1d, 10d));
  }

  @Test
  public void testToRange() throws Exception {
    assertEquals(5, MathUtil.toRange(5, 1, 10));
    assertEquals(1, MathUtil.toRange(1, 1, 10));
    assertEquals(10, MathUtil.toRange(10, 1, 10));
    assertEquals(1, MathUtil.toRange(Integer.MIN_VALUE, 1, 10));
    assertEquals(10, MathUtil.toRange(Integer.MAX_VALUE, 1, 10));
    assertEquals(5d, MathUtil.toRange(5d, 1d, 10d), 0d);
    assertEquals(1d, MathUtil.toRange(1d, 1d, 10d), 0d);
    assertEquals(10d, MathUtil.toRange(10d, 1d, 10d), 0d);
    assertEquals(1d, MathUtil.toRange(Integer.MIN_VALUE, 1d, 10d), 0d);
    assertEquals(10d, MathUtil.toRange(Integer.MAX_VALUE, 1d, 10d), 0d);
  }
}
