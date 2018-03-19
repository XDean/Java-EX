package xdean.jex.util.function;

import static org.junit.Assert.*;
import static xdean.jex.util.function.Predicates.*;

import org.junit.Test;

import xdean.jex.extra.collection.Wrapper;
import xdean.jex.util.function.Predicates;

public class PredicatesTest {
  @Test
  public void test() throws Exception {
    assertTrue(is(null).test(null));
    assertFalse(is(null).test(1));
    assertFalse(is(1000).test(1000));
    assertTrue(not(null).test(1));
    assertFalse(not(null).test(null));

    assertFalse(isEquals(new Object()).test(new Object()));
    assertTrue(isEquals(new String("1")).test("1"));
    assertFalse(notEquals(new String("1")).test("1"));
    assertTrue(notEquals(new Object()).test(new Object()));

    assertTrue(Predicates.<Wrapper<Integer>, Integer> its(Wrapper::get, isEquals(1)).test(new Wrapper<>(1)));
  }
}
