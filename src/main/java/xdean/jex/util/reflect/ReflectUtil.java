package xdean.jex.util.reflect;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@UtilityClass
public class ReflectUtil {

  private final Method GET_ROOT_METHODS;
  static {
    try {
      GET_ROOT_METHODS = Class.class.getDeclaredMethod("privateGetPublicMethods", new Class[] {});
      GET_ROOT_METHODS.setAccessible(true);
    } catch (NoSuchMethodException | SecurityException e) {
      throw new IllegalStateException("ReflectUtil init fail, check your java version.", e);
    }
  }

  /**
   * Get the actual root methods of the class.(BE CAREFUL)
   * 
   * @param clz
   * @return
   */
  public Method[] getRootMethods(Class<?> clz) {
    try {
      return (Method[]) GET_ROOT_METHODS.invoke(clz, new Object[] {});
    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
      throw new IllegalStateException(e);
    }
  }

  @SuppressWarnings("unchecked")
  public <T, O> O getField(Class<T> clz, T t, String fieldName) throws NoSuchFieldException {
    Field field = clz.getDeclaredField(fieldName);
    field.setAccessible(true);
    try {
      return (O) field.get(t);
    } catch (IllegalAccessException e) {
      log.error("Should not happen.", e);
      throw new IllegalStateException(e);
    }
  }

  public static Method getMethodBySignature(Class<?> clz, String methodSignature) throws NoSuchMethodException {
    Optional<Method> find = Stream.of(clz.getMethods()).filter(m -> m.toString().equals(methodSignature)).findAny();
    if (find.isPresent()) {
      return find.get();
    } else {
      throw new NoSuchMethodException(
          String.format("There is no method like %s in Class[%s]", methodSignature, clz.getName()));
    }
  }

  public static Method getGetter(Class<?> clz, String fieldName) throws NoSuchMethodException {
    try {
      return clz.getMethod(String.format("get%s%s", fieldName.toUpperCase().charAt(0), fieldName.substring(1)),
          new Class[] {});
    } catch (Exception e) {
      try {
        return clz.getMethod(String.format("is%s%s", fieldName.toUpperCase().charAt(0), fieldName.substring(1)),
            new Class[] {});
      } catch (Exception ee) {
        throw new NoSuchMethodException(
            String.format("There is no getter of %s in Class[%s]", fieldName, clz.getName()));
      }
    }
  }

  public static Method getSetter(Class<?> clz, String fieldName) throws NoSuchMethodException {
    Optional<Method> method = Stream.of(clz.getMethods())
        .filter(m -> m.getName()
            .equals(String.format("set%s%s", fieldName.toUpperCase().charAt(0), fieldName.substring(1))))
        .filter(m -> m.getReturnType().equals(Void.TYPE)).filter(m -> m.getParameterCount() == 1).findFirst();
    if (method.isPresent() == false) {
      throw new NoSuchMethodException(
          String.format("There is no setter of %s in Class[%s]", fieldName, clz.getName()));
    }
    return method.get();
  }

  public static Map<String, Object> beanToMap(Object bean) {
    Class<?> beanClass = bean.getClass();
    Map<String, Object> map = new HashMap<>();
    Field[] declaredFields = beanClass.getDeclaredFields();
    Arrays.stream(declaredFields).forEach(f -> {
      try {
        Method getter = getGetter(beanClass, f.getName());
        Object result = getter.invoke(bean, new Object[] {});
        map.put(f.getName(), result);
      } catch (Exception e) {
        log.error("", e);
      }
    });
    return map;
  }

  public static boolean isBasicType(Object object) {
    return isBasicType(object.getClass());
  }

  public static boolean isBasicType(Class<?> clz) {
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

  public static <T> Object parseBasicType(Class<T> clz, String objectValue) {
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
