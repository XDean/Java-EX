package cn.xdean.jex.reflect;

import org.junit.Test;

import static cn.xdean.jex.reflect.PrimitiveTypeUtil.*;
import static org.junit.Assert.*;

public class PrimitiveTypeUtilTest {
  @Test
  public void testIsPrimitive() {
    assertTrue(isPrimitive(int.class));
    assertTrue(isPrimitive(boolean.class));
    assertTrue(isPrimitive(long.class));
    assertTrue(isPrimitive(char.class));
    assertTrue(isPrimitive(byte.class));
    assertTrue(isPrimitive(float.class));
    assertTrue(isPrimitive(double.class));
    assertTrue(isPrimitive(short.class));
    assertFalse(isPrimitive(String.class));
    assertFalse(isPrimitive(PrimitiveTypeUtilTest.class));
  }

  @Test
  public void testIsPrimitiveArray() {
    assertTrue(isPrimitiveArray(int[].class));
    assertTrue(isPrimitiveArray(boolean[].class));
    assertTrue(isPrimitiveArray(long[].class));
    assertTrue(isPrimitiveArray(char[].class));
    assertTrue(isPrimitiveArray(byte[].class));
    assertTrue(isPrimitiveArray(float[].class));
    assertTrue(isPrimitiveArray(double[].class));
    assertTrue(isPrimitiveArray(short[].class));
    assertFalse(isPrimitiveArray(String[].class));
    assertFalse(isPrimitiveArray(PrimitiveTypeUtilTest[].class));
  }

  @Test
  public void testTransform() {
    assertEquals(Integer.class, toWrapper(int.class));
    assertEquals(int.class, toPrimitive(Integer.class));
    assertEquals(Integer[].class, toWrapperArray(int[].class));
    assertEquals(int[].class, toPrimitiveArray(Integer[].class));
  }

  @Test
  public void testToWrapperArray() throws Exception {
    int[] origin = { 1, 2, 3 };
    Integer[] result = (Integer[]) toWrapperArray(origin);
    assertEquals(Integer[].class, result.getClass());
    for (int i = 0; i < origin.length; i++) {
      assertEquals(origin[i], result[i].intValue());
    }
  }

  @Test
  public void testToWrapperArrayHighDimension() throws Exception {
    int[][] origin = {
        { 1, 2, 3 },
        { 4, 5, 6 },
        { 7, 8, 9 }
    };
    Integer[][] result = (Integer[][]) toWrapperArray(origin);
    assertEquals(Integer[][].class, result.getClass());
    for (int i = 0; i < origin.length; i++) {
      for (int m = 0; m < origin[i].length; m++) {
        assertEquals(origin[i][m], result[i][m].intValue());
      }
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testToWrapperArrayThrow() throws Exception {
    toWrapperArray(this);
  }

  @Test
  public void testToPrimitiveArray() throws Exception {
    Integer[] origin = { 1, 2, 3 };
    int[] result = (int[]) toPrimitiveArray(origin);
    assertEquals(int[].class, result.getClass());
    for (int i = 0; i < origin.length; i++) {
      assertEquals(origin[i].intValue(), result[i]);
    }
  }

  @Test
  public void testToPrimitiveArrayHighDimension() throws Exception {
    Integer[][] origin = {
        { 1, 2, 3 },
        { 4, 5, 6 },
        { 7, 8, 9 }
    };
    int[][] result = (int[][]) toPrimitiveArray(origin);
    assertEquals(int[][].class, result.getClass());
    for (int i = 0; i < origin.length; i++) {
      for (int m = 0; m < origin[i].length; m++) {
        assertEquals(origin[i][m].intValue(), result[i][m]);
      }
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testToPrimitiveArrayThrow() throws Exception {
    toPrimitiveArray(this);
  }
}
