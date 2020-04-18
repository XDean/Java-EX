package cn.xdean.jex.lang.function;

import cn.xdean.jex.lang.collection.Wrapper;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PredicatesTest {
  @Test
  public void test() throws Exception {
    assertTrue(Predicates.is(null).test(null));
    assertFalse(Predicates.is(null).test(1));
    assertFalse(Predicates.is(1000).test(1000));
    assertTrue(Predicates.not(null).test(1));
    assertFalse(Predicates.not(null).test(null));

    assertFalse(Predicates.isEquals(new Object()).test(new Object()));
    assertTrue(Predicates.isEquals(new String("1")).test("1"));
    assertFalse(Predicates.notEquals(new String("1")).test("1"));
    assertTrue(Predicates.notEquals(new Object()).test(new Object()));

    assertTrue(Predicates.<Wrapper<Integer>, Integer>its(Wrapper::get, Predicates.isEquals(1)).test(new Wrapper<>(1)));
  }
}
