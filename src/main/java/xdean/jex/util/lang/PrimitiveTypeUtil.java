package xdean.jex.util.lang;

import java.util.HashMap;
import java.util.Map;

public class PrimitiveTypeUtil {

  private static final Map<Class<?>, Class<?>> primitiveToWrappers = new HashMap<>();
  private static final Map<Class<?>, Class<?>> wrappersToPrimitives = new HashMap<>();
  static {
    add(boolean.class, Boolean.class);
    add(byte.class, Byte.class);
    add(char.class, Character.class);
    add(double.class, Double.class);
    add(float.class, Float.class);
    add(int.class, Integer.class);
    add(long.class, Long.class);
    add(short.class, Short.class);
    add(void.class, Void.class);
  }

  /**
   * Get wrapper class of primitive type. Or null for other.
   *
   * @param primitiveType
   * @return
   */
  public static Class<?> toWrapper(final Class<?> primitiveType) {
    return primitiveToWrappers.get(primitiveType);
  }

  /**
   * Get primitive type of wrapper class. Or null for other.
   *
   * @param wrapperType
   * @return
   */
  public static Class<?> toPrimitive(final Class<?> wrapperType) {
    return wrappersToPrimitives.get(wrapperType);
  }

  /**
   * Determine the class is primitive or not.
   *
   * @param clz
   * @return
   */
  public static boolean isPrimitive(Class<?> clz) {
    return primitiveToWrappers.keySet().contains(clz);
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
    primitiveToWrappers.put(primitiveType, wrapperType);
    wrappersToPrimitives.put(wrapperType, primitiveType);
  }
}
