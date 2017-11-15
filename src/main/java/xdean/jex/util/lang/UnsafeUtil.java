package xdean.jex.util.lang;

import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

import org.slf4j.Logger;

import sun.misc.Unsafe;
import xdean.jex.util.log.LogUtil;
import xdean.jex.util.reflect.ReflectUtil;

public class UnsafeUtil {

  private static final Unsafe THE_UNSAFE = initUnsafe();
  private static final int ADDRESS_SIZE = THE_UNSAFE.addressSize();
  private static final long HEADER_SIZE = initHeaderSize();
  private static final boolean useCompressedOops = initUseCompressedOops();
  private static final Logger LOGGER = LogUtil.log();
  static {
    LOGGER.debug("address size: {}", ADDRESS_SIZE);
    LOGGER.debug("header size: {}", HEADER_SIZE);
    LOGGER.debug("useCompressedOops: {}", useCompressedOops);
  }

  public static Unsafe getUnsafe() {
    return THE_UNSAFE;
  }

  /**
   * Get the memory address of the given object.
   *
   * @param o
   * @return
   */
  public static long addressOf(Object o) {
    Object[] array = new Object[] { o };
    long baseOffset = THE_UNSAFE.arrayBaseOffset(Object[].class);
    long objectAddress;
    switch (ADDRESS_SIZE) {
    case 4:
      objectAddress = THE_UNSAFE.getInt(array, baseOffset);
      break;
    case 8:
      objectAddress = THE_UNSAFE.getLong(array, baseOffset);
      break;
    default:
      throw new IllegalStateException("unsupported address size: " + ADDRESS_SIZE);
    }
    return objectAddress;
  }

  /**
   * Measure the size of the given class.
   *
   * @param clz
   * @return
   */
  public static long sizeOf(Class<?> clz) {
    if (PrimitiveTypeUtil.isPrimitive(clz)) {
      return PrimitiveTypeUtil.sizeOf(clz);
    } else {
      long maxOffset = 0L;
      Field maxField = null;
      for (Field field : ReflectUtil.getAllFields(clz, false)) {
        long offset = THE_UNSAFE.objectFieldOffset(field);
        if (offset > maxOffset) {
          maxOffset = offset;
          maxField = field;
        }
      }
      if (maxOffset == 0) {
        maxOffset = HEADER_SIZE;
      } else if (maxOffset < HEADER_SIZE) {
        throw new IllegalStateException("Can't calculate " + clz + "'s size.");
      } else {
        maxOffset += refSizeOf(maxField.getType());
      }
      return ((int) Math.ceil((double) maxOffset / ADDRESS_SIZE)) * ADDRESS_SIZE;
    }
  }

  /**
   * Get the size of reference.(considered UseCompressedOops)<br>
   * If you want to get the actual address size, use {@link Unsafe#addressSize()}
   *
   * @return ref size
   */
  public static long refSize() {
    return useCompressedOops ? 4 : ADDRESS_SIZE;
  }

  /**
   * Get the size of reference.(considered UseCompressedOops)<br>
   * If the class is primitive type, return it's actual size.
   *
   * @param clz
   * @return
   */
  public static long refSizeOf(Class<?> clz) {
    if (PrimitiveTypeUtil.isPrimitive(clz)) {
      return PrimitiveTypeUtil.sizeOf(clz);
    } else {
      return refSize();
    }
  }

  public static long getHeaderSize() {
    return HEADER_SIZE;
  }

  /**
   * Determined whether the vm use -XX:+UseCompressedOops.
   *
   * @return If it's running on a 32 bit vm, it also return true;
   */
  public static boolean isUsecompressedOops() {
    return useCompressedOops;
  }

  private static boolean initUseCompressedOops() {
    @SuppressWarnings("unused")
    class Helper {
      Object a, b;
    }
    try {
      Field a = Helper.class.getDeclaredField("a");
      Field b = Helper.class.getDeclaredField("b");
      long aOffset = THE_UNSAFE.objectFieldOffset(a);
      long bOffset = THE_UNSAFE.objectFieldOffset(b);
      return Math.abs(bOffset - aOffset) == 4;
    } catch (NoSuchFieldException e) {
      throw new Error("Never happen.", e);
    }
  }

  private static long initHeaderSize() {
    @SuppressWarnings("unused")
    class Helper {
      Object o;
    }
    Field[] fields = ReflectUtil.getAllFields(Helper.class, false);
    if (fields.length != 1) {
      throw new Error("Java Object don't behavior as expect. Check your java version or your code.");
    }
    Field f = fields[0];
    return THE_UNSAFE.objectFieldOffset(f);
  }

  private static Unsafe initUnsafe() {
    try {
      return Unsafe.getUnsafe();
    } catch (SecurityException tryReflectionInstead) {
      try {
        return AccessController.doPrivileged(
            (PrivilegedExceptionAction<Unsafe>) () -> {
              Class<Unsafe> k = Unsafe.class;
              for (Field f : k.getDeclaredFields()) {
                f.setAccessible(true);
                Object x = f.get(null);
                if (k.isInstance(x)) {
                  return k.cast(x);
                }
              }
              throw new NoSuchFieldError("the Unsafe");
            });
      } catch (PrivilegedActionException e) {
        throw new RuntimeException("Could not initialize intrinsics", e.getCause());
      }
    }
  }
}
