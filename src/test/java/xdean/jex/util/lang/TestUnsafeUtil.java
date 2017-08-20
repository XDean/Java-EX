package xdean.jex.util.lang;

import static org.junit.Assert.*;
import static xdean.jex.util.lang.UnsafeUtil.*;

import org.junit.Ignore;
import org.junit.Test;

public class TestUnsafeUtil {
  @Test
  public void testGetUnsafe() {
    assertNotNull(getUnsafe());
  }

  @Ignore("not implement yet")
  @Test
  public void testAddressOf() {
    System.out.printf("%s\n", Long.toHexString(addressOf(new Object())));
    System.out.printf("%s\n", Long.toHexString(addressOf(new Object())));
    System.out.printf("%s\n", Long.toHexString(addressOf(new Object())));
  }

  @Test
  public void testSizeOf() {
    if (System.getProperty("sun.arch.data.model").equals("64")) {
      if (UnsafeUtil.isUsecompressedOops()) {
        assertEquals(12, getHeaderSize());
        assertEquals(16, sizeOf(Object.class));
        assertEquals(32, sizeOf(SizeA.class));
        assertEquals(32, sizeOf(SizeB.class));
        assertEquals(24, sizeOf(SizeC.class));
        assertEquals(24, sizeOf(SizeD.class));
        assertEquals(16, sizeOf(SizeE.class));
      } else {
        assertEquals(16, getHeaderSize());
        assertEquals(16, sizeOf(Object.class));
        assertEquals(32, sizeOf(SizeA.class));
        assertEquals(40, sizeOf(SizeB.class));
        assertEquals(32, sizeOf(SizeC.class));
        assertEquals(40, sizeOf(SizeD.class));
        assertEquals(24, sizeOf(SizeE.class));
      }
    } else {
      throw new UnsupportedOperationException("Haven't test ont 32 bit yet.");
    }
  }

  static class SizeA {
    byte a;
    int b;
    byte c;
    long d;
    byte e;
  }

  static class SizeB extends SizeA {
    int f;
  }

  static class SizeC {
    int a;
    Object c;
  }

  static class SizeD {
    Object a, b, c;
  }

  static class SizeE {
    boolean a, b, c, d;
  }
}
