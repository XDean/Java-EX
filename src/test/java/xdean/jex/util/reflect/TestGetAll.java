package xdean.jex.util.reflect;

import static org.junit.Assert.*;
import io.reactivex.Observable;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;

import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings("unused")
public class TestGetAll {

  @Test
  public void testGetAllFields() {
    Observable.fromArray(ReflectUtil.getAllFields(B.class, false))
        .map(Field::getName)
        .test()
        .assertValueCount(2)
        .assertValues("b", "a");
  }

  @Ignore("Some tool will inject static fields.")
  @Test
  public void testGetAllFieldsStatic() {
    Observable.fromArray(ReflectUtil.getAllFields(B.class, true))
        .map(Field::getName)
        .test()
        .assertValueCount(3)
        .assertValues("b", "sa", "a");
  }

  @Test
  public void testGetAllMethods() {
    Observable.fromArray(ReflectUtil.getAllMethods(B.class))
        .filter(m -> m.getDeclaringClass() != Object.class)
        .map(Method::getName)
        .test()
        .assertValueCount(4)
        .assertValueSet(Arrays.asList("func", "func", "funcI", "funcM"));
  }

  @Test
  public void testGetAllSuperClasses() throws Exception {
    Observable.fromArray(ReflectUtil.getAllSuperClasses(B.class))
        .test()
        .assertValueCount(2)
        .assertValueSet(Arrays.asList(A.class, Object.class));
  }

  @Test
  public void testGetAllInterfaces() throws Exception {
    Observable.fromArray(ReflectUtil.getAllInterfaces(B.class))
        .test()
        .assertValueCount(2)
        .assertValueSet(Arrays.asList(I.class, M.class));
  }

  interface I {
    default void funcI() {
    }
  }

  interface M {
    default void funcM() {
    }
  }

  static class A implements I {
    private static int sa;
    private int a;

    void func() {

    }
  }

  static class B extends A implements M {
    private int b;

    @Override
    void func() {

    }
  }
}
