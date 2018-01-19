package xdean.jex.util.reflect;

import static org.junit.Assert.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Arrays;

import org.junit.Test;

public class GetRootTest {

  @Test
  public void testSetModifier() throws Exception {
    Constructor<TestClass> con = ReflectUtil.getRootExecutable(TestClass.class.getDeclaredConstructor());
    ReflectUtil.setModifiers(con, con.getModifiers() & ~Modifier.PRIVATE | Modifier.PUBLIC);
    TestClass.class.getDeclaredConstructor().newInstance();
  }

  @Test
  public void testGetRootExecutable() throws Exception {
    assertNotNull(ReflectUtil.getRootExecutable(TestClass.class.getDeclaredConstructor()));
    assertNotNull(ReflectUtil.getRootExecutable(TestClass.class.getConstructor(int.class)));
    assertNotNull(ReflectUtil.getRootMethod(TestClass.class.getDeclaredMethod("func")));
    assertNotNull(ReflectUtil.getRootMethod(TestClass.class.getMethod("bar")));
    assertEquals(1, Arrays.stream(ReflectUtil.getRootMethods(TestClass.class))
        .filter(m -> m.getDeclaringClass() == TestClass.class).count());
  }

  @Test
  public void testGetRootField() throws Exception {
    assertNotNull(ReflectUtil.getRootField(TestClass.class.getDeclaredField("fa")));
    assertNotNull(ReflectUtil.getRootField(TestClass.class.getDeclaredField("fb")));
    assertNotNull(ReflectUtil.getRootField(TestClass.class.getField("fc")));
    assertEquals(1, Arrays.stream(ReflectUtil.getRootFields(TestClass.class))
        .filter(m -> m.getDeclaringClass() == TestClass.class).count());
  }

  @SuppressWarnings("unused")
  private static class TestClass {
    private int fa;
    int[] fb;
    public Object fc;

    private TestClass() {
    }

    public TestClass(int i) {
    }

    void func() {
    }

    public void bar() {
    };
  }
}
