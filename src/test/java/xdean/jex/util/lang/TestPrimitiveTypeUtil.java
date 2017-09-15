package xdean.jex.util.lang;

import static org.junit.Assert.*;
import static xdean.jex.util.lang.PrimitiveTypeUtil.*;
import org.junit.Test;

public class TestPrimitiveTypeUtil {
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
    assertFalse(isPrimitive(TestPrimitiveTypeUtil.class));
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
    assertFalse(isPrimitiveArray(TestPrimitiveTypeUtil[].class));
  }

  @Test
  public void testTransform() {
    assertEquals(Integer.class, toWrapper(int.class));
    assertEquals(int.class, toPrimitive(Integer.class));
  }
}
