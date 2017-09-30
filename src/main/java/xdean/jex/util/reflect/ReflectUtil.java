package xdean.jex.util.reflect;

import static xdean.jex.util.lang.ExceptionUtil.uncheck;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ReflectUtil {

  private static final UnaryOperator<Method> METHOD_GET_ROOT;
  private static final Function<Class<?>, Method[]> CLASS_GET_ROOT_METHODS;
  private static final UnaryOperator<Field> FIELD_GET_ROOT;
  private static final Function<Class<?>, Field[]> CLASS_GET_ROOT_FIELDS;
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

  /**
   * Get field value by name
   *
   * @param clz
   * @param t
   * @param fieldName
   * @return
   * @throws NoSuchFieldException
   */
  @SuppressWarnings("unchecked")
  public static <T, O> O getFieldValue(Class<T> clz, T t, String fieldName) throws NoSuchFieldException {
    Field field = clz.getDeclaredField(fieldName);
    field.setAccessible(true);
    try {
      return (O) field.get(t);
    } catch (IllegalAccessException e) {
      log.error("Should not happen.", e);
      throw new IllegalStateException(e);
    }
  }

  class IntList extends ArrayList<Integer> {
  }

  /**
   * Get the actual generic types.<br>
   * For example:
   *
   * <pre>
   * <code>
   * class IntList extends ArrayList<Integer>{}
   * getGenericType(IntList.class, List.class);// {Integer.class}
   * getGenericType(IntList.class, Collection.class);// {Integer.class}
   * getGenericType(Integer.class, Comparable.class);// {Integer.class}
   * </code>
   * </pre>
   *
   * And even nested situation
   *
   * <pre>
   * <code>
   * class A<E,T>{}
   * class B<E> extends A<E,Integer>{}
   * class C extends<Boolean>{}
   * getGenericType(C.class, A.class);// {Boolean.class, Integer.class}
   * getGenericType(B.class, A.class);// {null, Integer.class}
   * </code>
   * </pre>
   *
   * @param clz The implementation class.
   * @param targetClass Find the actual generic type on this type.
   * @return An array with Class object. Its length equals targetClass's generic parameters' length. If a generic
   *         parameter is still generic, it will be null in the returned array.
   */
  public static Class<?>[] getGenericTypes(Class<?> clz, Class<?> targetClass) {
    boolean isInterface = targetClass.isInterface();
    TypeVariable<?>[] targetTypeParameters = targetClass.getTypeParameters();
    log.debug("Type paramter length: {}", targetTypeParameters.length);
    if (targetTypeParameters.length == 0) {
      return new Class<?>[0];
    }
    // Not conflict yet type parameters
    List<TypeVariable<?>> leftTypeParameters = Arrays.asList(clz.getTypeParameters());
    // Possible class of conflict type definition
    List<Class<?>> classes = new ArrayList<>();
    classes.add(clz);
    classes.addAll(Arrays.asList(getAllSuperClasses(clz)));
    if (isInterface) {
      classes.addAll(Arrays.asList(getAllInterfaces(clz)));
    }
    // Map for type reference
    // Value is always Class(the conflict type) or TypeVariable(the reference type)
    Map<TypeVariable<?>, Type> actualTypeMap = new HashMap<>();
    for (Class<?> c : classes) {
      List<Type> generics = new ArrayList<>();
      generics.add(c.getGenericSuperclass());
      if (isInterface) {
        generics.addAll(Arrays.asList(c.getGenericInterfaces()));
      }
      generics.removeIf(t -> t == null);
      // For every generic type
      for (Type t : generics) {
        if (t instanceof ParameterizedType) {
          ParameterizedType parameterizedType = (ParameterizedType) t;
          Type rawType = parameterizedType.getRawType();
          if (!(rawType instanceof Class)) {
            log.info("Unkown raw type: {} with type {}.", rawType, rawType.getClass());
            continue;
          }
          Class<?> rawClz = (Class<?>) rawType;
          Type[] genericTypes = parameterizedType.getActualTypeArguments();
          // If the raw type is target class, resolve it and return
          if (rawClz.equals(targetClass)) {
            log.debug("Find generic types: {}", (Object) genericTypes);
            List<Class<?>> ret = new ArrayList<>();
            for (Type type : genericTypes) {
              // If is conflict, add directly
              if (type instanceof Class) {
                ret.add((Class<?>) type);
              } else if (type instanceof TypeVariable) {
                // If it's just still generic, add null
                if (leftTypeParameters.contains(type)) {
                  ret.add(null);
                }
                // / If it's generic from subclass, find the conflict type from subclass(the type reference map)
                else {
                  Type actualType = type;
                  while (actualType instanceof TypeVariable) {
                    actualType = actualTypeMap.get(actualType);
                  }
                  if (actualType != null) {
                    ret.add((Class<?>) actualType);
                  } else {
                    ret.add(null);
                    log.error("A type variable neither explicit or generic: {} with type {}", type, type.getClass());
                  }
                }
              } else {
                log.warn("Unknown actual type argument: {} with type {}", type.getClass());
              }
            }
            return ret.toArray(new Class<?>[ret.size()]);
          }
          // If not, record conflict generic or generic reference in current class
          else {
            TypeVariable<?>[] typeParameters = rawClz.getTypeParameters();
            for (int i = 0; i < typeParameters.length; i++) {
              if ((genericTypes[i] instanceof Class<?>) || (genericTypes[i] instanceof TypeVariable)) {
                Type previous = actualTypeMap.putIfAbsent(typeParameters[i], genericTypes[i]);
                if (previous != null) {
                  log.warn("There already has type map(which should not happen): {} -> {}", typeParameters[i], previous);
                }
              } else {
              }
            }
          }
        } else {
          log.info("Unknown generic type: {} with type {}.", t, t.getClass());
        }
      }
    }
    return new Class<?>[0];
  }

  /**
   * Get all fields
   *
   * @param clz
   * @param includeStatic include static fields or not
   * @return
   */
  public static Field[] getAllFields(Class<?> clz, boolean includeStatic) {
    return Arrays.stream(getAllFields(clz))
        .filter(f -> includeStatic || !Modifier.isStatic(f.getModifiers()))
        .toArray(Field[]::new);
  }

  /**
   * Get all fields
   *
   * @param clz
   * @return
   */
  public static Field[] getAllFields(Class<?> clz) {
    List<Field> list = new ArrayList<>();
    do {
      list.addAll(Arrays.asList(clz.getDeclaredFields()));
    } while ((clz = clz.getSuperclass()) != null);
    return list.toArray(new Field[list.size()]);
  }

  /**
   * Get all methods
   *
   * @param clz
   * @return
   */
  public static Method[] getAllMethods(Class<?> clz) {
    Set<Method> set = new HashSet<>();
    List<Class<?>> classes = new ArrayList<>();
    classes.add(clz);
    classes.addAll(Arrays.asList(getAllSuperClasses(clz)));
    classes.addAll(Arrays.asList(getAllInterfaces(clz)));
    for (Class<?> c : classes) {
      set.addAll(Arrays.asList(c.getDeclaredMethods()));
    }
    return set.toArray(new Method[set.size()]);
  }

  /**
   * Get all super classes
   *
   * @param clz
   * @return
   */
  public static Class<?>[] getAllSuperClasses(Class<?> clz) {
    List<Class<?>> list = new ArrayList<>();
    while ((clz = clz.getSuperclass()) != null) {
      list.add(clz);
    }
    return list.toArray(new Class<?>[list.size()]);
  }

  /**
   * Get all interfaces
   *
   * @param clz
   * @return
   */
  public static Class<?>[] getAllInterfaces(Class<?> clz) {
    HashSet<Class<?>> set = new HashSet<>();
    getAllInterfaces(clz, set);
    return set.toArray(new Class<?>[set.size()]);
  }

  private static void getAllInterfaces(Class<?> clz, Set<Class<?>> visited) {
    if (clz.getSuperclass() != null) {
      getAllInterfaces(clz.getSuperclass(), visited);
    }
    for (Class<?> c : clz.getInterfaces()) {
      if (visited.add(c)) {
        getAllInterfaces(c, visited);
      }
    }
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
    return getCaller().getClassName();
  }

  public static StackTraceElement getCaller() {
    return getCaller(1, true);
  }

  /**
   * Get caller stack info.
   *
   * @param deep Deep to search the caller class.If deep is 0, it returns the class who calls this method.
   * @param ignoreSameClass If it is true, calling in same class will be ignored.
   * @return
   */
  public static StackTraceElement getCaller(int deep, boolean ignoreSameClass) {
    // index 0 is Thread.getStackTrace
    // index 1 is ReflectUtil.getCallerClassName
    StackTraceElement[] stElements = Thread.currentThread().getStackTrace();
    StackTraceElement currentStack = stElements[1];
    int found = deep + 1;
    for (int i = 1; i < stElements.length; i++) {
      StackTraceElement nextStack = stElements[i];
      if (nextStack.getClassName().equals(ReflectUtil.class.getName())) {
        continue;
      }
      if (!ignoreSameClass || !currentStack.getClassName().equals(nextStack.getClassName())) {
        currentStack = nextStack;
        found--;
      }
      if (found == 0) {
        return currentStack;
      }
    }
    return null;
  }
}
