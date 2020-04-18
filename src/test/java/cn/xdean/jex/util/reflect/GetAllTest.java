package cn.xdean.jex.util.reflect;

import io.reactivex.Observable;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;

import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings("unused")
public class GetAllTest {

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

  @Ignore("Some tool will inject static methods.")
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

  @Test
  public void testGetRootFields() throws Exception {
    Observable.fromArray(ReflectUtil.getRootFields(C.class))
        .filter(f -> ReflectUtil.getRootField(f) == null)
        .map(Field::getName)
        .test()
        .assertValueCount(1)
        .assertValues("a");
  }

  @Test
  public void testGetRootMethods() throws Exception {
    Observable.fromArray(ReflectUtil.getRootMethods(D.class))
        .filter(m -> ReflectUtil.getRootMethod(m) == null)
        .filter(m -> m.getDeclaringClass() != Object.class)
        .map(Method::getName)
        .test()
        .assertValueCount(2)
        .assertValueSet(Arrays.asList("bar", "funcM"));
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

  static class C {
    public int a;
    private int b;

    void bar() {
    }
  }

  static class D extends C implements M {
    public int c;

    @Override
    public void bar() {
    }
  }
}
