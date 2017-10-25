package xdean.jex.util.lang;

import static org.junit.Assert.*;
import static xdean.jex.util.lang.ArrayUtil.*;

import org.junit.Test;

public class ArrayUtilTest {
  @Test
  public void testTransposeSquare() throws Exception {
    int[][] origin = {
        { 1, 2, 3 },
        { 4, 5, 6 },
        { 7, 8, 9 }
    };
    int[][] result = {
        { 1, 4, 7 },
        { 2, 5, 8 },
        { 3, 6, 9 }
    };
    assertArrayEquals(result, transpose(origin));
  }

  @Test
  public void testTransposeNotSquare() throws Exception {
    int[][] origin = {
        { 1, 2, 3 },
        { 4, 5 },
        { 7 }
    };
    int[][] result = {
        { 1, 4, 7 },
        { 2, 5 },
        { 3 }
    };
    assertArrayEquals(result, transpose(origin));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testTransposeThrow() throws Exception {
    int[][] origin = {
        { 1, 2, 3 },
        { 4, 5 },
        { 7, 8, 9 }
    };
    transpose(origin);
  }

  @Test
  public void testCompareInt() throws Exception {
    assertTrue(compare(new int[] { 1, 2, 3 }, new int[] { 1, 2, 3 }) == 0);
    assertTrue(compare(new int[] { 1, 2, 3 }, new int[] { 1, 2, 5 }) < 0);
    assertTrue(compare(new int[] { 1, 3, 3 }, new int[] { 1, 2, 5 }) > 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCompareIntThrow() throws Exception {
    assertTrue(compare(new int[] { 1, 2, 3 }, new int[] { 1, 2 }) == 0);
  }

  @Test
  public void testCompare() throws Exception {
    assertTrue(compare(new Integer[] { 1, 2, 3 }, new Integer[] { 1, 2, 3 }) == 0);
    assertTrue(compare(new Integer[] { 1, 2, 3 }, new Integer[] { 1, 2, 5 }) < 0);
    assertTrue(compare(new Integer[] { 1, 3, 3 }, new Integer[] { 1, 2, 5 }) > 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCompareThrow() throws Exception {
    assertTrue(compare(new Integer[] { 1, 2, 3 }, new Integer[] { 1, 2 }) == 0);
  }
}
