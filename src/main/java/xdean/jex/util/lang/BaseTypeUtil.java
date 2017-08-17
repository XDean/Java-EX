package xdean.jex.util.lang;

public class BaseTypeUtil {
  public static boolean isBaseType(Class<?> clz) {
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

  public static <T> Object parseBaseType(Class<T> clz, String objectValue) {
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
