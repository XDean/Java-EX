package xdean.jex.util.lang;

public class PrimitiveTypeUtil {
  public static boolean isPrimitive(Class<?> clz) {
    switch (clz.getName()) {
    case "int":
    case "short":
    case "long":
    case "double":
    case "float":
    case "boolean":
    case "char":
    case "byte":
      return true;
    default:
      return false;
    }
  }

  public static int sizeOf(Class<?> clz) {
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

  public static <T> Object parse(Class<T> clz, String objectValue) {
    switch (clz.getName()) {
    case "int":
      return Integer.valueOf(objectValue);
    case "short":
      return Short.valueOf(objectValue);
    case "long":
      if (objectValue.endsWith("L") || objectValue.endsWith("l")) {
        objectValue = objectValue.substring(0, objectValue.length() - 1);
      }
      return Long.valueOf(objectValue);
    case "double":
      return Double.valueOf(objectValue);
    case "float":
      return Float.valueOf(objectValue);
    case "boolean":
      if (objectValue.equalsIgnoreCase(Boolean.TRUE.toString())) {
        return Boolean.TRUE;
      } else if (objectValue.equalsIgnoreCase(Boolean.FALSE.toString())) {
        return Boolean.FALSE;
      } else {
        throw new IllegalArgumentException(
            String.format("The String %s cannot parse as boolean.", objectValue));
      }
    case "char":
      if (objectValue.length() == 1) {
        return new Character(objectValue.charAt(0));
      } else {
        throw new IllegalArgumentException(String.format("The String %s cannot parse as char.", objectValue));
      }
    case "byte":
      return Byte.valueOf(objectValue);
    default:
      return null;
    }
  }
}
