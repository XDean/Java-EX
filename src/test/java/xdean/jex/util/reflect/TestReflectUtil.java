package xdean.jex.util.reflect;

import io.reactivex.Observable;

import java.lang.reflect.Field;
import java.util.Arrays;

import org.junit.Test;

@SuppressWarnings("unused")
public class TestReflectUtil {
  @Test
  public void testGetAllMethod() {
    Observable.fromArray(ReflectUtil.getAllFields(B.class, false))
        .map(Field::getName)
        .test()
        .assertValueCount(2)
        .assertValues("b", "a");
  }

  @Test
  public void testGetAllMethodStatic() {
    Observable.fromArray(ReflectUtil.getAllFields(B.class, true))
        .map(Field::getName)
        .test()
        .assertValueCount(3)
        .assertValues("b", "sa", "a");
  }

  static class A {
    private static int sa;
    private int a;
  }

  static class B extends A {
    private int b;
  }
}
