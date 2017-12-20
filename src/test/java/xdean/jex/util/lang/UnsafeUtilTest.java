package xdean.jex.util.lang;

import static org.junit.Assert.*;
import static xdean.jex.util.lang.UnsafeUtil.*;
import static xdean.jex.util.task.TaskUtil.andFinal;

import org.junit.Ignore;
import org.junit.Test;

import xdean.jex.extra.Wrapper;

public class UnsafeUtilTest {

  @Test
  public void testGetUnsafe() {
    assertNotNull(getUnsafe());
  }

  @Ignore("not implement yet")
  @Test
  public void testAddressOf() {
    // System.out.printf("%s\n", Long.toHexString(addressOf(new Object())));
    // System.out.printf("%s\n", Long.toHexString(addressOf(new Object())));
    // System.out.printf("%s\n", Long.toHexString(addressOf(new Object())));
  }

  @Test
  public void testSizeOf() {
    if (System.getProperty("sun.arch.data.model").equals("64")) {
      assertSize(12, 16, getHeaderSize());
      assertSize(16, 16, shallowSizeOf(Object.class));
      assertSize(32, 32, shallowSizeOf(SizeA.class));
      assertSize(32, 40, shallowSizeOf(SizeB.class));
      assertSize(24, 32, shallowSizeOf(SizeC.class));
      assertSize(24, 40, shallowSizeOf(SizeD.class));
      assertSize(16, 24, shallowSizeOf(SizeE.class));
    } else {
      throw new UnsupportedOperationException("Haven't test ont 32 bit yet.");
    }
  }

  @Test
  public void testSizeOfObject() {
    if (System.getProperty("sun.arch.data.model").equals("64")) {
      assertSize(12, 16, getHeaderSize());
      assertSize(16, 16, sizeOf(new Object()));
      assertSize(32, 32, sizeOf(new SizeA()));
      assertSize(32, 40, sizeOf(new SizeB()));
      assertSize(24, 32, sizeOf(new SizeC()));
      assertSize(24, 40, sizeOf(new SizeD()));
      assertSize(16, 24, sizeOf(new SizeE()));
      assertSize(32, 40, sizeOf(new int[] { 1, 2, 3 }));
      assertSize(56, 72, sizeOf(new Object[] { new Object(), new Object() }));
      assertSize(16, 24, sizeOf(Wrapper.empty()));
      assertSize(32, 40, sizeOf(Wrapper.of(new Object())));
      assertSize(16, 24, sizeOf(andFinal(() -> Wrapper.empty(), w -> w.set(w))));
    } else {
      throw new UnsupportedOperationException("Haven't test ont 32 bit yet.");
    }
  }

  private static void assertSize(long useCompressedOop, long notUse, long actual) {
    if (UnsafeUtil.isUsecompressedOops()) {
      assertEquals(useCompressedOop, actual);
    } else {
      assertEquals(notUse, actual);
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
