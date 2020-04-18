package cn.xdean.jex.lang;

import org.junit.Test;

import static cn.xdean.jex.lang.collection.ArrayUtil.*;
import static org.junit.Assert.*;

public class ArrayUtilTest {

  @Test
  public void testDeepClone() throws Exception {
    int[][] origin = {
        { 1, 2, 3 },
        { 4, 5, 6 },
        { 7, 8, 9 }
    };
    int[][] clone = (int[][]) deepClone(origin);
    assertArrayEquals(origin, clone);
    origin[2][2] = 100;
    assertEquals(9, clone[2][2]);
  }

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
