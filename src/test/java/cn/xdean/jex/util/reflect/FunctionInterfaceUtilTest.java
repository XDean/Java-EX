package cn.xdean.jex.util.reflect;

import static org.junit.Assert.*;
import static cn.xdean.jex.util.reflect.FunctionInterfaceUtil.*;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings("unchecked")
public class FunctionInterfaceUtilTest {

  @Test
  public void testFunctionInterface() throws Exception {
    assertNotNull(getFunctionInterfaceMethod(Function.class));
    assertNotNull(getFunctionInterfaceMethod(Predicate.class));
    assertNotNull(getFunctionInterfaceMethod(Runnable.class));
    assertNotNull(getFunctionInterfaceMethod(UnaryOperator.class));
    assertNull(getFunctionInterfaceMethod(Object.class));
    assertNull(getFunctionInterfaceMethod(List.class));
    assertNull(getFunctionInterfaceMethod(Cloneable.class));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testLonger() throws Exception {
    methodToFunctionInterface(get("runnable"), null, Runnable.class, Object.class);
  }

  @Test
  public void testStatic() throws Exception {
    Function<Integer, Integer> iuo = methodToFunctionInterface(get("staticIntToInt"), null, Function.class);
    assertEquals(2, iuo.apply(1).intValue());
  }

  @Test(expected = NullPointerException.class)
  public void testMethodGeneric() throws Exception {
    GenericInter gi = methodToFunctionInterface(get("objToObj"), null, GenericInter.class);
    Runnable o = () -> gi.getClass();
    assertEquals(o, gi.get(o));
  }

  @Test
  public void testMethodGeneric2() throws Exception {
    GenericInter gi = methodToFunctionInterface(get("objToObj2"), null, GenericInter.class);
    Runnable o = () -> gi.getClass();
    assertEquals(o, gi.get(o));
  }

  @Test
  public void testExplicitType() throws Exception {
    UnaryOperator<Integer> uo = methodToFunctionInterface(get("mapInt"), null, UnaryOperator.class, Integer.class);
    assertEquals(1, uo.apply(1).intValue());
  }

  @Test
  public void testExplicitTypeNull() throws Exception {
    UnaryOperator<Double> uo = methodToFunctionInterface(get("mapDouble"), null, UnaryOperator.class, (Class<?>) null);
    assertEquals(1d, uo.apply(1d).doubleValue(), 0);
  }

  @Test(expected = NullPointerException.class)
  public void testExplicitTypeFail() throws Exception {
    UnaryOperator<Double> uo = methodToFunctionInterface(get("mapDouble"), null, UnaryOperator.class, Integer.class);
    assertEquals(1, uo.apply(1d).intValue());
  }

  @Ignore("Can't handle ParameterType mismatch now")
  @Test(expected = NullPointerException.class)
  public void testParameterTypes() throws Exception {
    Function<List<Integer>, String> func = methodToFunctionInterface(get("stringList"), null, Function.class);
    assertEquals("[1]", func.apply(Arrays.asList(1)));
  }

  private static Method get(String name) {
    return Arrays.stream(A.class.getDeclaredMethods())
        .filter(m -> m.getName().equals(name))
        .findFirst()
        .get();
  }

  interface GenericInter {
    <T extends Runnable> T get(T t);
  }

  static class A {
    public static void runnable() {
    }

    public static int staticIntToInt(int i) {
      return i + 1;
    }

    public static <T> T objToObj(T t) {
      return t;
    }

    public static <T extends Runnable> T objToObj2(T t) {
      return t;
    }

    public static int mapInt(Integer i) {
      return i;
    }

    public static double mapDouble(Double d) {
      return d;
    }

    public static String stringList(List<String> list) {
      return list.toString();
    }
  }
}
