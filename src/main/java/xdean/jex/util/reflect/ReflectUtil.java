package xdean.jex.util.reflect;

import static xdean.jex.util.task.TaskUtil.uncheck;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ReflectUtil {

  public static final UnaryOperator<Method> METHOD_GET_ROOT;
  public static final Function<Class<?>, Method[]> CLASS_GET_ROOT_METHODS;
  public static final UnaryOperator<Field> FIELD_GET_ROOT;
  public static final Function<Class<?>, Field[]> CLASS_GET_ROOT_FIELDS;
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
  public static Method getRootMethod(Method m) {
    return METHOD_GET_ROOT.apply(m);
  }

  /**
   * Get root methods of the class.
   */
  public static Method[] getRootMethods(Class<?> clz) {
    return CLASS_GET_ROOT_METHODS.apply(clz);
  }

  /**
   * Get root of the field.
   */
  public static Field getRootField(Field f) {
    return FIELD_GET_ROOT.apply(f);
  }

  /**
   * Get root fields of the class.
   */
  public static Field[] getRootFields(Class<?> clz) {
    return CLASS_GET_ROOT_FIELDS.apply(clz);
  }

  @SuppressWarnings("unchecked")
  public static <T, O> O getField(Class<T> clz, T t, String fieldName) throws NoSuchFieldException {
    Field field = clz.getDeclaredField(fieldName);
    field.setAccessible(true);
    try {
      return (O) field.get(t);
    } catch (IllegalAccessException e) {
      log.error("Should not happen.", e);
      throw new IllegalStateException(e);
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

  /**
   * Get the name of the class who calls the caller.<br>
   * For example. The following code will print "A".
   *
   * <pre>
   * <code>class A {
   *   static void fun() {
   *     B.fun();
   *   }
   * }
   * 
   * class B {
   *   static void fun() {
   *     System.out.println(ReflectUtil.getCallerClassName());
   *   }
   * }</code>
   * </pre>
   *
   * @return
   */
  public static String getCallerClassName() {
    return getCallerClassName(2, true);
  }

  /**
   * Get caller class.
   *
   * @param deep Deep to search the caller class.If deep is 0, it returns ReflectUtil.class itself. If deep is 1, it
   *          returns the class who calls this method.
   * @param ignoreSame If it is true, calling in same class will be ignored.
   * @return
   */
  public static String getCallerClassName(int deep, boolean ignoreSame) {
    StackTraceElement[] stElements = Thread.currentThread().getStackTrace();
    String currentClassName = ReflectUtil.class.getName();
    int found = deep;
    // index 0 is Thread.getStackTrace
    // index 1 is ReflectUtil.getCallerClassName
    for (int i = 2; i < stElements.length; i++) {
      if (found == 0) {
        return currentClassName;
      }
      StackTraceElement ste = stElements[i];
      if (!(ignoreSame && currentClassName.equals(ste.getClassName()))) {
        currentClassName = ste.getClassName();
        found--;
      }
    }
    return null;
  }
}
