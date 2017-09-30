package xdean.jex.util.lang;

import java.lang.reflect.Array;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

public class PrimitiveTypeUtil {

  private static final BiMap<Class<?>, Class<?>> wrapperToPrimitive = HashBiMap.create();
  private static final BiMap<Class<?>, Class<?>> arrayToPrimitive = HashBiMap.create();
  static {
    add(boolean.class, Boolean.class);
    add(byte.class, Byte.class);
    add(char.class, Character.class);
    add(double.class, Double.class);
    add(float.class, Float.class);
    add(int.class, Integer.class);
    add(long.class, Long.class);
    add(short.class, Short.class);
  }

  public static Object toWrapper(Object o) {
    return o;
  }

  /**
   * Convert a primitive type array to wrapper type array.<br>
   * If not primitive array, return itself.<br>
   * If not array, throw IllegalArgumentException.
   *
   * @param array
   * @return
   */
  public static Object toWrapperArray(Object array) {
    if (!array.getClass().isArray()) {
      throw new IllegalArgumentException("Must give array object.");
    }
    if (isPrimitiveArray(array.getClass())) {
      int length = Array.getLength(array);
      Object newArray = Array.newInstance(toWrapper(array.getClass().getComponentType()), length);
      for (int i = 0; i < length; i++) {
        Array.set(newArray, i, toWrapper(Array.get(array, i)));
      }
      return newArray;
    }
    return array;
  }

  /**
   * Get wrapper class of primitive type. Or itself for other.
   *
   * @param primitiveType
   * @return
   */
  public static Class<?> toWrapper(final Class<?> primitiveType) {
    return wrapperToPrimitive.inverse().getOrDefault(primitiveType, primitiveType);
  }

  /**
   * Get primitive type of wrapper class. Or itself for other.
   *
   * @param wrapperType
   * @return
   */
  public static Class<?> toPrimitive(final Class<?> wrapperType) {
    return wrapperToPrimitive.getOrDefault(wrapperType, wrapperType);
  }

  /**
   * Determine the class is primitive or not.
   *
   * @param clz
   * @return
   */
  public static boolean isPrimitive(Class<?> clz) {
    return wrapperToPrimitive.inverse().keySet().contains(clz);
  }

  /**
   * Determine the class is primitive array or not.
   *
   * @param clz
   * @return
   */
  public static boolean isPrimitiveArray(Class<?> clz) {
    return arrayToPrimitive.keySet().contains(clz);
  }

  /**
   * Determine the class is primitive wrapper or not.
   *
   * @param clz
   * @return
   */
  public static boolean isWrapper(Class<?> clz) {
    return wrapperToPrimitive.keySet().contains(clz);
  }

  /**
   * Get the size of a primitive type.
   *
   * @param clz
   * @throws IllegalArgumentException If the class is not primitive type.
   * @return
   */
  public static int sizeOf(Class<?> clz) throws IllegalArgumentException {
    switch (clz.getName()) {
    case "int":
      return Integer.BYTES;
    case "short":
      return Short.BYTES;
    case "long":
      return Long.BYTES;
    case "double":
      return Double.BYTES;
    case "float":
      return Float.BYTES;
    case "boolean":
      return 1;
    case "char":
      return Character.BYTES;
    case "byte":
      return 1;
    default:
      throw new IllegalArgumentException("Not a primitive type.");
    }
  }

  /**
   * Parse the string value to a primitive type
   *
   * @param clz
   * @param objectValue
   * @return a wrapper type value
   */
  @SuppressWarnings("unchecked")
  public static <T> T parse(Class<T> clz, String objectValue) {
    switch (clz.getName()) {
    case "int":
      return (T) Integer.valueOf(objectValue);
    case "short":
      return (T) Short.valueOf(objectValue);
    case "long":
      if (objectValue.endsWith("L") || objectValue.endsWith("l")) {
        objectValue = objectValue.substring(0, objectValue.length() - 1);
      }
      return (T) Long.valueOf(objectValue);
    case "double":
      return (T) Double.valueOf(objectValue);
    case "float":
      return (T) Float.valueOf(objectValue);
    case "boolean":
      if (objectValue.equalsIgnoreCase(Boolean.TRUE.toString())) {
        return (T) Boolean.TRUE;
      } else if (objectValue.equalsIgnoreCase(Boolean.FALSE.toString())) {
        return (T) Boolean.FALSE;
      } else {
        throw new IllegalArgumentException(
            String.format("The String %s cannot parse as boolean.", objectValue));
      }
    case "char":
      if (objectValue.length() == 1) {
        return (T) new Character(objectValue.charAt(0));
      } else {
        throw new IllegalArgumentException(String.format("The String %s cannot parse as char.", objectValue));
      }
    case "byte":
      return (T) Byte.valueOf(objectValue);
    default:
      return null;
    }
  }

  private static void add(final Class<?> primitiveType, final Class<?> wrapperType) {
    wrapperToPrimitive.put(wrapperType, primitiveType);
    arrayToPrimitive.put(Array.newInstance(primitiveType, 1).getClass(), primitiveType);
  }
}
