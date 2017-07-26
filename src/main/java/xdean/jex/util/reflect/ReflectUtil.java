package xdean.jex.util.reflect;

import static xdean.jex.util.task.TaskUtil.uncheck;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@UtilityClass
public class ReflectUtil {

  private final UnaryOperator<Method> METHOD_GET_ROOT;
  private final Function<Class<?>, Method[]> CLASS_GET_ROOT_METHODS;
  private final UnaryOperator<Field> FIELD_GET_ROOT;
  private final Function<Class<?>, Field[]> CLASS_GET_ROOT_FIELDS;
  static {
    try {
      Method getRootMethod = Method.class.getDeclaredMethod("getRoot");
      getRootMethod.setAccessible(true);
      METHOD_GET_ROOT = m -> uncheck(() -> (Method) getRootMethod.invoke(m));
      Method getRootMethods = Class.class.getDeclaredMethod("privateGetPublicMethods");
      getRootMethods.setAccessible(true);
      CLASS_GET_ROOT_METHODS = c -> uncheck(() -> (Method[]) getRootMethods.invoke(c));

      Field getRootField = Field.class.getDeclaredField("root");
      getRootField.setAccessible(true);
      FIELD_GET_ROOT = f -> uncheck(() -> (Field) getRootField.get(f));
      Method getRootFields = Class.class.getDeclaredMethod("privateGetPublicMethods");
      getRootFields.setAccessible(true);
      CLASS_GET_ROOT_FIELDS = c -> uncheck(() -> (Field[]) getRootFields.invoke(c));
    } catch (NoSuchMethodException | SecurityException | NoSuchFieldException e) {
      throw new IllegalStateException("ReflectUtil init fail, check your java version.", e);
    }
  }

  /**
   * Get root of the method.
   */
  public Method getRootMethod(Method m) {
    return METHOD_GET_ROOT.apply(m);
  }

  /**
   * Get root methods of the class.
   */
  public Method[] getRootMethods(Class<?> clz) {
    return CLASS_GET_ROOT_METHODS.apply(clz);
  }

  /**
   * Get root of the field.
   */
  public Field getRootField(Field f) {
    return FIELD_GET_ROOT.apply(f);
  }

  /**
   * Get root fields of the class.
   */
  public Field[] getRootFields(Class<?> clz) {
    return CLASS_GET_ROOT_FIELDS.apply(clz);
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
