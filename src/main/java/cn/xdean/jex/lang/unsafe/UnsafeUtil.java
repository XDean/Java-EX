package cn.xdean.jex.lang.unsafe;

import cn.xdean.jex.reflect.PrimitiveTypeUtil;
import cn.xdean.jex.reflect.ReflectUtil;
import sun.misc.Unsafe;
import xdean.jex.log.Log;
import xdean.jex.log.LogFactory;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.stream.Stream;

import static cn.xdean.jex.lang.ExceptionUtil.uncheck;

public class UnsafeUtil {

  private static final Unsafe THE_UNSAFE = initUnsafe();
  private static final int ADDRESS_SIZE = THE_UNSAFE.addressSize();
  private static final long HEADER_SIZE = initHeaderSize();
  private static final boolean useCompressedOops = initUseCompressedOops();
  private static final Log LOGGER = LogFactory.from(UnsafeUtil.class);
  static {
    LOGGER.trace("address size: {0}", ADDRESS_SIZE);
    LOGGER.trace("header size: {0}", HEADER_SIZE);
    LOGGER.trace("useCompressedOops: {0}", useCompressedOops);
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
   * Measure the SHALLOW size of the given class's instance.<br>
   * If given an array class, only return the base offset (size of array of length 0).<br>
   * For array class, use {@link UnsafeUtil#shallowSizeOf(Object)}.
   *
   * @param clz
   * @return
   */
  public static long shallowSizeOf(Class<?> clz) {
    if (PrimitiveTypeUtil.isPrimitive(clz)) {
      return PrimitiveTypeUtil.sizeOf(clz);
    } else if (clz.isArray()) {
      return THE_UNSAFE.arrayBaseOffset(clz);
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
      return regulateSize(maxOffset);
    }
  }

  /**
   * SHALLOW size of the given object. <br>
   * If given a primitive type, the result will be incorrect.<br>
   * For primitive type, use {@link UnsafeUtil#shallowSizeOf(Class)}.
   *
   * @param o
   * @return
   */
  public static long shallowSizeOf(Object o) {
    Class<? extends Object> clz = o.getClass();
    if (clz.isArray()) {
      int len = Array.getLength(o);
      return regulateSize(THE_UNSAFE.arrayBaseOffset(clz) + (long) THE_UNSAFE.arrayIndexScale(clz) * len);
    }
    return shallowSizeOf(clz);
  }

  /**
   * HEAP size of the given object. <br>
   * If given a primitive type, the result will be incorrect because auto-box.<br>
   * For primitive type, use {@link UnsafeUtil#shallowSizeOf(Class)}.
   *
   * @param o
   * @return
   */
  public static long sizeOf(Object o) {
    return sizeOf(o, new IdentityHashMap<>());
  }

  private static long sizeOf(Object o, Map<Object, Object> visited) {
    if (visited.keySet().contains(o)) {
      return 0L;
    }
    visited.put(o, null);
    if (o == null) {
      return 0;
    }
    Class<? extends Object> clz = o.getClass();
    if (clz.isArray()) {
      long len = Array.getLength(o);
      long shallowSize = shallowSizeOf(o);
      if (PrimitiveTypeUtil.isPrimitiveArray(clz)) {
        return shallowSize;
      } else {
        long size = shallowSize;
        for (int i = 0; i < len; i++) {
          size += sizeOf(Array.get(o, i), visited);
        }
        return size;
      }
    } else {
      return shallowSizeOf(o) + Stream.of(uncheck(() -> ReflectUtil.getAllFields(clz, false)))
          .mapToLong(f -> {
            if (PrimitiveTypeUtil.isPrimitive(f.getType())) {
              return 0;
            } else {
              f.setAccessible(true);
              return uncheck(() -> sizeOf(f.get(o), visited));
            }
          })
          .sum();
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

  private static long regulateSize(long size) {
    return size / ADDRESS_SIZE * ADDRESS_SIZE + (size % ADDRESS_SIZE == 0 ? 0 : ADDRESS_SIZE);
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
