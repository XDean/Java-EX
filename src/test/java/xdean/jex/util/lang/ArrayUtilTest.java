package xdean.jex.util.lang;

import static org.junit.Assert.*;

import org.junit.Test;

public class ArrayUtilTest {
  @Test
  public void testCompareInt() throws Exception {
    assertTrue(ArrayUtil.compare(new int[] { 1, 2, 3 }, new int[] { 1, 2, 3 }) == 0);
    assertTrue(ArrayUtil.compare(new int[] { 1, 2, 3 }, new int[] { 1, 2, 5 }) < 0);
    assertTrue(ArrayUtil.compare(new int[] { 1, 3, 3 }, new int[] { 1, 2, 5 }) > 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCompareIntThrow() throws Exception {
    assertTrue(ArrayUtil.compare(new int[] { 1, 2, 3 }, new int[] { 1, 2 }) == 0);
  }

  @Test
  public void testCompare() throws Exception {
    assertTrue(ArrayUtil.compare(new Integer[] { 1, 2, 3 }, new Integer[] { 1, 2, 3 }) == 0);
    assertTrue(ArrayUtil.compare(new Integer[] { 1, 2, 3 }, new Integer[] { 1, 2, 5 }) < 0);
    assertTrue(ArrayUtil.compare(new Integer[] { 1, 3, 3 }, new Integer[] { 1, 2, 5 }) > 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCompareThrow() throws Exception {
    assertTrue(ArrayUtil.compare(new Integer[] { 1, 2, 3 }, new Integer[] { 1, 2 }) == 0);
  }
}
