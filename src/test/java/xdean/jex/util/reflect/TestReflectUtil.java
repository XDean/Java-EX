package xdean.jex.util.reflect;

import static org.junit.Assert.*;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import org.junit.Test;

public class TestReflectUtil {
  @Test
  public void testFunctionInterface() throws Exception {
    assertNotNull(ReflectUtil.getFunctionInterfaceMethod(Function.class));
    assertNotNull(ReflectUtil.getFunctionInterfaceMethod(Predicate.class));
    assertNotNull(ReflectUtil.getFunctionInterfaceMethod(Runnable.class));
    assertNull(ReflectUtil.getFunctionInterfaceMethod(Object.class));
    assertNull(ReflectUtil.getFunctionInterfaceMethod(List.class));
    assertNull(ReflectUtil.getFunctionInterfaceMethod(Cloneable.class));
  }
}
