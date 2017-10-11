package xdean.jex.util.reflect;

import io.reactivex.Observable;

import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;

import org.junit.Test;

public class TestGeneric {

  private static Type NULL = new Type() {
    @Override
    public String toString() {
      return "NULL";
    };
  };

  private Type[] handleNull(Type[] array) {
    return Arrays.stream(array)
        .map(clz -> clz == null ? NULL : clz)
        .toArray(Type[]::new);
  }

  @Test
  public void test1() throws Exception {
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C1.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(NULL);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C1.class, I2.class)))
        .test()
        .assertValueCount(2)
        .assertValues(getTV(C1.class, 0), O1.class);
  }

  @Test
  public void test2() throws Exception {
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C2.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(O2.class);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C2.class, I2.class)))
        .test()
        .assertValueCount(2)
        .assertValues(getTV(C2.class, 0), O1.class);
  }

  @Test
  public void test3() throws Exception {
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C3.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(O2.class);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C3.class, I2.class)))
        .test()
        .assertValueCount(2)
        .assertValues(O1.class, O1.class);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C3.class, C1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C3.class, C2.class)))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
  }

  @Test
  public void test4() throws Exception {
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C4.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(NULL);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C4.class, I2.class)))
        .test()
        .assertValueCount(2)
        .assertValues(O1.class, O2.class);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C4.class, I3.class)))
        .test()
        .assertValueCount(3)
        .assertValues(O1.class, O2.class, getTV(C4.class, 0));
  }

  @Test
  public void test5() {
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C3.class.getGenericSuperclass(), C1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C3.class.getGenericSuperclass(), C2.class)))
        .test()
        .assertValueCount(1)
        .assertValues(O1.class);
  }

  @Test
  public void test6() {
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C5.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(
            GenericUtil.createParameterizedType(I2.class, null, getTV(C5.class, 0), Object.class));
    Observable.fromArray(handleNull(GenericUtil.getGenericTypes(C6.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(GenericUtil.createParameterizedType(I2.class, null, C6.class, Object.class));
  }

  @Test
  public void test7() {
    // I2<C5<? extends T>, Object>
    Observable
        .fromArray(handleNull(GenericUtil.getGenericTypes(C7.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(GenericUtil.createParameterizedType(I2.class, null,
            GenericUtil.createParameterizedType(C5.class, null,
                GenericUtil.createWildcardType(new Type[] { getTV(C7.class, 0) }, null)),
            Object.class));
    // I2<C5<? extends C8>, Object>
    Observable
        .fromArray(handleNull(GenericUtil.getGenericTypes(C8.class, I1.class)))
        .test()
        .assertValueCount(1)
        .assertValues(GenericUtil.createParameterizedType(I2.class, null,
            GenericUtil.createParameterizedType(C5.class, null,
                GenericUtil.createWildcardType(new Type[] { C8.class }, null)),
            Object.class));
  }

  private <T> TypeVariable<Class<T>> getTV(Class<T> clz, int i) {
    return clz.getTypeParameters()[i];
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

  static class C5<A> implements I1<I2<A, Object>> {
  }

  static class C6 extends C5<C6> {
  }

  static class C7<T> extends C5<C5<? extends T>> {
  }

  static class C8 extends C7<C8> {
  }
}
