package xdean.jex.util.reflect;

import static xdean.jex.util.lang.ExceptionUtil.uncheck;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import xdean.jex.extra.Pair;

/**
 * Change annotations in runtime.
 *
 * @author XDean
 *
 */
public class AnnotationUtil {

  private static final Constructor<?> AnnotationInvocationHandler_constructor;
  private static final Constructor<?> AnnotationData_constructor;
  private static final Method Class_annotationData;
  private static final Field Class_classRedefinedCount;
  private static final Field AnnotationData_annotations;
  private static final Field AnnotationData_declaredAnotations;
  private static final Method Atomic_casAnnotationData;
  private static final Class<?> Atomic_class;
  private static final Field Field_Excutable_DeclaredAnnotations;
  private static final Field Field_Field_DeclaredAnnotations;

  static {
    // static initialization of necessary reflection Objects
    try {
      Class<?> AnnotationInvocationHandler_class = Class.forName("sun.reflect.annotation.AnnotationInvocationHandler");
      AnnotationInvocationHandler_constructor = AnnotationInvocationHandler_class.getDeclaredConstructor(
          new Class[] { Class.class, Map.class });
      AnnotationInvocationHandler_constructor.setAccessible(true);

      Atomic_class = Class.forName("java.lang.Class$Atomic");
      Class<?> AnnotationData_class = Class.forName("java.lang.Class$AnnotationData");

      AnnotationData_constructor = AnnotationData_class.getDeclaredConstructor(
          new Class[] { Map.class, Map.class, int.class });
      AnnotationData_constructor.setAccessible(true);
      Class_annotationData = Class.class.getDeclaredMethod("annotationData");
      Class_annotationData.setAccessible(true);

      Class_classRedefinedCount = Class.class.getDeclaredField("classRedefinedCount");
      Class_classRedefinedCount.setAccessible(true);

      AnnotationData_annotations = AnnotationData_class.getDeclaredField("annotations");
      AnnotationData_annotations.setAccessible(true);
      AnnotationData_declaredAnotations = AnnotationData_class.getDeclaredField("declaredAnnotations");
      AnnotationData_declaredAnotations.setAccessible(true);

      Atomic_casAnnotationData = Atomic_class.getDeclaredMethod("casAnnotationData",
          Class.class, AnnotationData_class, AnnotationData_class);
      Atomic_casAnnotationData.setAccessible(true);

      Field_Excutable_DeclaredAnnotations = Executable.class.getDeclaredField("declaredAnnotations");
      Field_Excutable_DeclaredAnnotations.setAccessible(true);

      Field_Field_DeclaredAnnotations = Field.class.getDeclaredField("declaredAnnotations");
      Field_Field_DeclaredAnnotations.setAccessible(true);
    } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | NoSuchFieldException e) {
      throw new IllegalStateException("AnnotationUtil init fail, check your java version.", e);
    }
  }

  /**
   * Changes the annotation value for the given key of the given annotation to newValue and returns the previous value.
   *
   * @author Balder@stackoverflow
   * @see https://stackoverflow.com/a/28118436/7803527
   * @see sun.reflect.annotation.AnnotationInvocationHandler
   */
  @SuppressWarnings("unchecked")
  public static Object changeAnnotationValue(Annotation annotation, String key, Object newValue) {
    Object handler = Proxy.getInvocationHandler(annotation);
    Field f;
    try {
      f = handler.getClass().getDeclaredField("memberValues");
    } catch (NoSuchFieldException | SecurityException e) {
      throw new IllegalStateException(e);
    }
    f.setAccessible(true);
    Map<String, Object> memberValues;
    try {
      memberValues = (Map<String, Object>) f.get(handler);
    } catch (IllegalArgumentException | IllegalAccessException e) {
      throw new IllegalStateException(e);
    }
    Object oldValue = memberValues.get(key);
    if (oldValue == null || oldValue.getClass() != newValue.getClass()) {
      throw new IllegalArgumentException();
    }
    memberValues.put(key, newValue);
    return oldValue;
  }

  /**
   * Add annotation to Executable(Method or Constructor)<br>
   * Note that you may need to give the root method.
   *
   * @param ex
   * @param annotation
   * @author XDean
   * @see java.lang.reflect.Excutable
   * @see #createAnnotationFromMap(Class, Map)
   * @see ReflectUtil#getRootMethods(Class)
   */
  @SuppressWarnings("unchecked")
  public static void addAnnotation(Executable ex, Annotation annotation) {
    ex.getAnnotation(Annotation.class);// prevent declaredAnnotations haven't initialized
    Map<Class<? extends Annotation>, Annotation> annos;
    try {
      annos = (Map<Class<? extends Annotation>, Annotation>) Field_Excutable_DeclaredAnnotations.get(ex);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException(e);
    }
    if (annos.getClass() == Collections.EMPTY_MAP.getClass()) {
      annos = new HashMap<>();
      try {
        Field_Excutable_DeclaredAnnotations.set(ex, annos);
      } catch (IllegalAccessException e) {
        throw new IllegalStateException(e);
      }
    }
    annos.put(annotation.annotationType(), annotation);
  }

  @SuppressWarnings("unchecked")
  public static <T extends Annotation> T removeAnnotation(Executable ex, Class<T> annotationType) {
    if (ex.getAnnotation(annotationType) == null) {
      return null;
    }
    ex.getAnnotation(Annotation.class);// prevent declaredAnnotations haven't initialized
    Map<Class<? extends Annotation>, Annotation> annos;
    try {
      annos = (Map<Class<? extends Annotation>, Annotation>) Field_Excutable_DeclaredAnnotations.get(ex);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException(e);
    }
    return (T) annos.remove(annotationType);
  }

  /**
   * Add annotation to Field<br>
   * Note that you may need to give the root field.
   *
   * @param field
   * @param annotation
   * @author XDean
   * @see java.lang.reflect.Field
   * @see #createAnnotationFromMap(Class, Map)
   * @see ReflectUtil#getRootFields(Class)
   */
  @SuppressWarnings("unchecked")
  public static void addAnnotation(Field field, Annotation annotation) {
    field.getAnnotation(Annotation.class);// prevent declaredAnnotations haven't initialized
    Map<Class<? extends Annotation>, Annotation> annos;
    try {
      annos = (Map<Class<? extends Annotation>, Annotation>) Field_Field_DeclaredAnnotations.get(field);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException(e);
    }
    if (annos.getClass() == Collections.EMPTY_MAP.getClass()) {
      annos = new HashMap<>();
      try {
        Field_Field_DeclaredAnnotations.set(field, annos);
      } catch (IllegalAccessException e) {
        throw new IllegalStateException(e);
      }
    }
    annos.put(annotation.annotationType(), annotation);
  }

  @SuppressWarnings("unchecked")
  public static <T extends Annotation> T removeAnnotation(Field field, Class<T> annotationType) {
    if (field.getAnnotation(annotationType) == null) {
      return null;
    }
    field.getAnnotation(Annotation.class);// prevent declaredAnnotations haven't initialized
    Map<Class<? extends Annotation>, Annotation> annos;
    try {
      annos = (Map<Class<? extends Annotation>, Annotation>) Field_Field_DeclaredAnnotations.get(field);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException(e);
    }
    return (T) annos.remove(annotationType);
  }

  /**
   * @param c
   * @param annotation
   * @author Balder@stackoverflow
   * @see https://stackoverflow.com/a/30287201/7803527
   * @see java.lang.Class
   * @see #createAnnotationFromMap(Class, Map)
   */
  @SuppressWarnings("unchecked")
  public static <T extends Annotation> void addAnnotation(Class<?> c, T annotation) {
    try {
      while (true) { // retry loop
        int classRedefinedCount = Class_classRedefinedCount.getInt(c);
        Object /* AnnotationData */ annotationData = Class_annotationData.invoke(c);
        // null or stale annotationData -> optimistically create new instance
        Object newAnnotationData = changeClassAnnotationData(c, annotationData, (Class<T>) annotation.annotationType(),
            annotation, classRedefinedCount, true).getLeft();
        // try to install it
        if ((boolean) Atomic_casAnnotationData.invoke(Atomic_class, c, annotationData, newAnnotationData)) {
          // successfully installed new AnnotationData
          break;
        }
      }
    } catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException | InstantiationException e) {
      throw new IllegalStateException(e);
    }
  }

  public static <T extends Annotation> T removeAnnotation(Class<?> c, Class<T> annotationType) {
    if (c.getAnnotation(annotationType) == null) {
      return null;
    }
    try {
      while (true) {
        int classRedefinedCount = Class_classRedefinedCount.getInt(c);
        Object annotationData = Class_annotationData.invoke(c);
        Pair<Object, T> pair = changeClassAnnotationData(c, annotationData, annotationType, null,
            classRedefinedCount, false);
        Object newAnnotationData = pair.getLeft();
        if ((boolean) Atomic_casAnnotationData.invoke(Atomic_class, c, annotationData, newAnnotationData)) {
          return pair.getRight();
        }
      }
    } catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException | InstantiationException e) {
      throw new IllegalStateException(e);
    }
  }

  @SuppressWarnings("unchecked")
  private static <T extends Annotation> Pair<Object, T> changeClassAnnotationData(Class<?> c, Object annotationData,
      Class<T> annotationType, T annotation, int classRedefinedCount, boolean isAdd)
      throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    Map<Class<? extends Annotation>, Annotation> annotations = (Map<Class<? extends Annotation>, Annotation>) AnnotationData_annotations
        .get(annotationData);
    Map<Class<? extends Annotation>, Annotation> declaredAnnotations = (Map<Class<? extends Annotation>, Annotation>) AnnotationData_declaredAnotations
        .get(annotationData);

    Map<Class<? extends Annotation>, Annotation> newDeclaredAnnotations = new LinkedHashMap<>(annotations);
    T removed = null;
    if (isAdd) {
      newDeclaredAnnotations.put(annotationType, annotation);
    } else {
      removed = (T) newDeclaredAnnotations.remove(annotationType);
    }
    Map<Class<? extends Annotation>, Annotation> newAnnotations;
    if (declaredAnnotations == annotations) {
      newAnnotations = newDeclaredAnnotations;
    } else {
      newAnnotations = new LinkedHashMap<>(annotations);
      if (isAdd) {
        newAnnotations.put(annotationType, annotation);
      } else {
        newAnnotations.remove(annotationType);
      }
    }
    return Pair.of(AnnotationData_constructor.newInstance(newAnnotations, newDeclaredAnnotations, classRedefinedCount),
        removed);
  }

  /**
   * Create annotation from the given map.
   *
   * @param annotationClass
   * @param valuesMap
   * @return
   */
  @SuppressWarnings("unchecked")
  public static <T extends Annotation> T createAnnotationFromMap(Class<T> annotationClass,
      Map<String, Object> valuesMap) {
    Map<String, Object> map = getAnnotationDefaultMap(annotationClass);
    map.putAll(valuesMap);
    return AccessController
        .doPrivileged((PrivilegedAction<T>) () -> (T) Proxy.newProxyInstance(
            annotationClass.getClassLoader(),
            new Class[] { annotationClass },
            uncheck(
                () -> (InvocationHandler) AnnotationInvocationHandler_constructor.newInstance(annotationClass, map))));
  }

  public static <T extends Annotation> Map<String, Object> getAnnotationDefaultMap(Class<T> annotationClass) {
    return Stream.of(annotationClass.getDeclaredMethods())
        .filter(m -> m.getDefaultValue() != null)
        .collect(Collectors.toMap(m -> m.getName(), m -> m.getDefaultValue()));
  }
}
