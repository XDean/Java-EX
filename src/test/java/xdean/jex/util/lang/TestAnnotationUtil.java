package xdean.jex.util.lang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

import org.junit.Test;

import xdean.jex.util.reflect.AnnotationUtil;

public class TestAnnotationUtil {
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  public @interface TestAnnotation {
    String value();
  }

  public static class TestClass {
  }

  @Test
  public void test() {
    TestAnnotation annotation = TestClass.class.getAnnotation(TestAnnotation.class);
    assertNull(annotation);

    String value = "RuntimeValue";
    Map<String, Object> valuesMap = new HashMap<>();
    valuesMap.put("value", value);
    AnnotationUtil.addAnnotation(TestClass.class,
        AnnotationUtil.createAnnotationFromMap(TestAnnotation.class, valuesMap));

    annotation = TestClass.class.getAnnotation(TestAnnotation.class);
    assertNotNull(annotation);
    assertEquals(value, annotation.value());
  }
}
