package xdean.jex.util.lang;

public class BaseTypeUtil {

  public static boolean isBaseType(Object object) {
    return isBaseType(object.getClass());
  }

  public static boolean isBaseType(Class<?> clz) {
    switch (clz.getName()) {
    case "int":
    case "java.lang.Integer":
    case "short":
    case "java.lang.Short":
    case "long":
    case "java.lang.Long":
    case "double":
    case "java.lang.Double":
    case "float":
    case "java.lang.Float":
    case "boolean":
    case "java.lang.Boolean":
    case "char":
    case "java.lang.Character":
    case "byte":
    case "java.lang.Byte":
      return true;
    default:
      return false;
    }
  }

  public static <T> Object parseBaseType(Class<T> clz, String objectValue) {
    switch (clz.getName()) {
    case "int":
    case "java.lang.Integer":
      return Integer.valueOf(objectValue);
    case "short":
    case "java.lang.Short":
      return Short.valueOf(objectValue);
    case "long":
    case "java.lang.Long":
      if (objectValue.endsWith("L") || objectValue.endsWith("l")) {
        objectValue = objectValue.substring(0, objectValue.length() - 1);
      }
      return Long.valueOf(objectValue);
    case "double":
    case "java.lang.Double":
      return Double.valueOf(objectValue);
    case "float":
    case "java.lang.Float":
      return Float.valueOf(objectValue);
    case "boolean":
    case "java.lang.Boolean":
      if (objectValue.equalsIgnoreCase(Boolean.TRUE.toString())) {
        return Boolean.TRUE;
      } else if (objectValue.equalsIgnoreCase(Boolean.FALSE.toString())) {
        return Boolean.FALSE;
      } else {
        throw new IllegalArgumentException(
            String.format("The String %s cannot parse as boolean.", objectValue));
      }
    case "char":
    case "java.lang.Character":
      if (objectValue.length() == 1) {
        return new Character(objectValue.charAt(0));
      } else {
        throw new IllegalArgumentException(String.format("The String %s cannot parse as char.", objectValue));
      }
    case "byte":
    case "java.lang.Byte":
      return Byte.valueOf(objectValue);
    case "java.lang.String":
      return objectValue;
    case "void":
      return Void.TYPE;
    default:
      return null;
    }
  }
}
