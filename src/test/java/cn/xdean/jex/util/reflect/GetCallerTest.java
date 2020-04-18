package cn.xdean.jex.util.reflect;

import static org.junit.Assert.*;

import org.junit.Test;

public class GetCallerTest {
  @Test
  public void tetGetCallerName() {
    assertEquals(GetCallerTest.class.getName(), First.fisrt(0).getClassName());
    assertEquals(First.class.getName(), First.fisrt(1).getClassName());
    assertEquals(Second.class.getName(), First.fisrt(2).getClassName());
    assertEquals(Second.class.getName(), First.fisrt(3).getClassName());
    assertEquals(Second.class.getName(), First.fisrt(4).getClassName());
    assertEquals(Second.class.getName(), First.fisrt(5).getClassName());
  }

  @Test
  public void testIgnore() {
    assertEquals(Third.class.getName(), First.first(5, 0, false).getClassName());
    assertEquals(Third.class.getName(), First.first(5, 1, false).getClassName());
    assertEquals(Third.class.getName(), First.first(5, 2, false).getClassName());
    assertEquals(Third.class.getName(), First.first(5, 3, false).getClassName());
    assertEquals(Second.class.getName(), First.first(5, 4, false).getClassName());
    assertEquals(First.class.getName(), First.first(5, 5, false).getClassName());
    assertEquals(GetCallerTest.class.getName(), First.first(5, 6, false).getClassName());

    assertEquals(Third.class.getName(), First.first(5, 0, true).getClassName());
    assertEquals(Second.class.getName(), First.first(5, 1, true).getClassName());
    assertEquals(First.class.getName(), First.first(5, 2, true).getClassName());
    assertEquals(GetCallerTest.class.getName(), First.first(5, 3, true).getClassName());
  }

  // First ( -> Second -> Third*)?
  static class First {
    static StackTraceElement fisrt(int count) {
      if (count == 0) {
        return ReflectUtil.getCaller();
      }
      return Second.second(--count);
    }

    static StackTraceElement first(int count, int deep, boolean ignoreSame) {
      if (count == 0) {
        return ReflectUtil.getCaller(deep, ignoreSame);
      }
      return Second.second(--count, deep, ignoreSame);
    }
  }

  static class Second {
    static StackTraceElement second(int count) {
      if (count == 0) {
        return ReflectUtil.getCaller();
      }
      return Third.third(--count);
    }

    static StackTraceElement second(int count, int deep, boolean ignoreSame) {
      if (count == 0) {
        return ReflectUtil.getCaller(deep, ignoreSame);
      }
      return Third.third(--count, deep, ignoreSame);
    }
  }

  static class Third {
    static StackTraceElement third(int count) {
      if (count == 0) {
        return ReflectUtil.getCaller();
      }
      return Third.third(--count);
    }

    static StackTraceElement third(int count, int deep, boolean ignoreSame) {
      if (count == 0) {
        return ReflectUtil.getCaller(deep, ignoreSame);
      }
      return Third.third(--count, deep, ignoreSame);
    }
  }

}
