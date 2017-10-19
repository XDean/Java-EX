package xdean.jex.util.reflect;

import static org.junit.Assert.*;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

import org.junit.Test;

public class TypeUtilTest {
  @Test
  public void testAssignToPrimitive() throws Exception {
    assertTrue(TypeUtil.isAssignableFrom(int.class, int.class));
    assertFalse(TypeUtil.isAssignableFrom(Integer.class, int.class));
    assertFalse(TypeUtil.isAssignableFrom(Object.class, int.class));
  }

  @Test
  public void testAssignClass() throws Exception {
    assertFalse(TypeUtil.isAssignableFrom(int.class, Collection.class));
    assertFalse(TypeUtil.isAssignableFrom(Integer.class, Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(List.class, Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(ArrayList.class, Collection.class));
  }

  @Test
  public void testAssignParameterizedType() throws Exception {
    assertTrue(TypeUtil.isAssignableFrom(
        GenericUtil.createParameterizedType(List.class, null, Integer.class),
        Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(
        GenericUtil.createParameterizedType(ArrayList.class, null, Integer.class),
        Collection.class));
    assertFalse(TypeUtil.isAssignableFrom(
        GenericUtil.createParameterizedType(Callable.class, null, Integer.class),
        Collection.class));
  }

  @Test
  public void testAssignTypeVariable() throws Exception {
    assertFalse(TypeUtil.isAssignableFrom(getTV(TV.class, 0), Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(getTV(TV.class, 1), Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(getTV(TV.class, 2), Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(getTV(TV.class, 3), Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(
        ((ParameterizedType) TV2.class.getGenericSuperclass()).getActualTypeArguments()[3], Collection.class));
  }

  @Test
  public void testAssignWildcardType() throws Exception {
    assertFalse(TypeUtil.isAssignableFrom(
        GenericUtil.createWildcardType(new Type[] { Integer.class }, new Type[0]),
        Collection.class));
    assertFalse(TypeUtil.isAssignableFrom(
        GenericUtil.createWildcardType(new Type[0], new Type[] { ArrayList.class }),
        Collection.class));
    assertTrue(TypeUtil.isAssignableFrom(
        GenericUtil.createWildcardType(new Type[] { ArrayList.class }, new Type[0]),
        Collection.class));
  }

  private <T extends GenericDeclaration> TypeVariable<?> getTV(T clz, int i) {
    return clz.getTypeParameters()[i];
  }

  class TV<A extends Number, B extends List<Callable<? extends A>>, C extends B, D extends C> {
  }

  class TV2<D extends ArrayList<Callable<? extends Number>>> extends
      TV<Number, List<Callable<? extends Number>>, ArrayList<Callable<? extends Number>>, D> {
  }
}
