package cn.xdean.jex.reflect;

import org.junit.Test;
import org.junit.runners.Suite.SuiteClasses;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static cn.xdean.jex.reflect.AnnotationUtil.*;
import static org.junit.Assert.*;

public class AnnotationUtilTest {
  private static final String VALUE = "value";
  private static final String BEFORE = "before";
  private static final String AFTER = "after";

  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  public @interface TestAnnotation {
    String value();
  }

  @SuiteClasses({})
  public static class Parent {
    public int i;
  }

  public static class TestClass extends Parent {
    public void func() {
    }
  }

  private TestAnnotation annotation;

  @Test
  public void testClass() {
    annotation = TestClass.class.getAnnotation(TestAnnotation.class);
    assertNull(annotation);

    // add
    Map<String, Object> valuesMap = new HashMap<>();
    valuesMap.put(VALUE, BEFORE);
    addAnnotation(TestClass.class, createAnnotationFromMap(TestAnnotation.class, valuesMap));
    annotation = TestClass.class.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(BEFORE, annotation.value());

    // change
    changeAnnotationValue(annotation, VALUE, AFTER);
    annotation = TestClass.class.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(AFTER, annotation.value());

    // remove
    annotation = removeAnnotation(TestClass.class, TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(AFTER, annotation.value());
    annotation = TestClass.class.getAnnotation(TestAnnotation.class);
    assertNull(annotation);
  }

  @Test
  public void testMethod() throws NoSuchMethodException, SecurityException {
    Method method = TestClass.class.getMethod("func");
    annotation = method.getAnnotation(TestAnnotation.class);
    assertNull(annotation);

    // add
    Map<String, Object> valuesMap = new HashMap<>();
    valuesMap.put(VALUE, BEFORE);
    addAnnotation(method, createAnnotationFromMap(TestAnnotation.class, valuesMap));
    annotation = method.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(BEFORE, annotation.value());

    // change
    changeAnnotationValue(annotation, VALUE, AFTER);
    annotation = method.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(AFTER, annotation.value());

    // remove
    annotation = removeAnnotation(method, TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(AFTER, annotation.value());
    annotation = method.getAnnotation(TestAnnotation.class);
    assertNull(annotation);
  }

  @Test
  public void testField() throws NoSuchFieldException, SecurityException {
    Field field = TestClass.class.getField("i");
    annotation = field.getAnnotation(TestAnnotation.class);
    assertNull(annotation);

    // add
    Map<String, Object> valuesMap = new HashMap<>();
    valuesMap.put(VALUE, BEFORE);
    addAnnotation(field, createAnnotationFromMap(TestAnnotation.class, valuesMap));
    annotation = field.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(BEFORE, annotation.value());

    // change
    changeAnnotationValue(annotation, VALUE, AFTER);
    annotation = field.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(AFTER, annotation.value());

    // remove
    annotation = removeAnnotation(field, TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(AFTER, annotation.value());
    annotation = field.getAnnotation(TestAnnotation.class);
    assertNull(annotation);
  }

  @Test
  public void testCopy() throws Exception {
    annotation = createAnnotationFromMap(TestAnnotation.class, Collections.singletonMap(VALUE, VALUE));
    TestAnnotation copy = copyAnnotation(annotation);
    assertNotSame(annotation, copy);
    assertEquals(annotation, copy);
  }
}
