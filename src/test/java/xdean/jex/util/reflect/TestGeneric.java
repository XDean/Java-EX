package xdean.jex.util.reflect;

import io.reactivex.Observable;

import java.util.Arrays;

import org.junit.Test;

public class TestGeneric {

  private static Class<?> NULL = TestGeneric.class;

  private Class<?>[] handleNull(Class<?>[] array) {
    return Arrays.stream(array)
        .map(clz -> clz == null ? NULL : clz)
        .toArray(Class<?>[]::new);
  }

  @Test
  public void test1() throws Exception {
    Observable.fromArray(ReflectUtil.getGenericTypes(C1.class, I1.class))
        .test()
        .assertValueCount(0);
    Observable.fromArray(handleNull(ReflectUtil.getGenericTypes(C1.class, I2.class)))
        .test()
        .assertValueCount(2)
        .assertValues(NULL, O1.class);
  }

  @Test
  public void test2() throws Exception {
    Observable.fromArray(ReflectUtil.getGenericTypes(C2.class, I1.class))
        .test()
        .assertValueCount(1)
        .assertValues(O2.class);
    Observable.fromArray(handleNull(ReflectUtil.getGenericTypes(C2.class, I2.class)))
        .test()
        .assertValueCount(2)
        .assertValues(NULL, O1.class);
  }

  @Test
  public void test3() throws Exception {
    Observable.fromArray(ReflectUtil.getGenericTypes(C3.class, I1.class))
        .test()
        .assertValueCount(1)
        .assertValues(O2.class);
    Observable.fromArray(ReflectUtil.getGenericTypes(C3.class, I2.class))
        .test()
        .assertValueCount(2)
        .assertValues(O1.class, O1.class);
    Observable.fromArray(ReflectUtil.getGenericTypes(C3.class, C1.class))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
    Observable.fromArray(ReflectUtil.getGenericTypes(C3.class, C2.class))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
  }

  @Test
  public void test4() throws Exception {
    Observable.fromArray(ReflectUtil.getGenericTypes(C4.class, I1.class))
        .test()
        .assertValueCount(0);
    Observable.fromArray(ReflectUtil.getGenericTypes(C4.class, I2.class))
        .test()
        .assertValueCount(2)
        .assertValues(O1.class, O2.class);
    Observable.fromArray(ReflectUtil.getGenericTypes(C4.class, I3.class))
        .test()
        .assertValueCount(2)
        .assertValues(O1.class, O2.class);
  }

  @Test
  public void test5() {
    Observable.fromArray(ReflectUtil.getGenericTypes(C3.class.getGenericSuperclass(), C1.class))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
    Observable.fromArray(ReflectUtil.getGenericTypes(C3.class.getGenericSuperclass(), C2.class))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
  }

  static class O1 {
  }

  static class O2 {
  };

  interface I1<A> {
  }

  interface I2<A, B> {
  }

  interface I3<A, B, C> extends I2<A, B> {
  }

  static class C1<A> implements I2<A, O1> {
  }

  static class C2<A> extends C1<A> implements I1<O2> {
  }

  static class C3 extends C2<O1> {
  }

  static class C4<C> implements I3<O1, O2, C> {
  }
}
