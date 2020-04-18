package cn.xdean.jex.extra;

import java.util.function.Predicate;

import cn.xdean.jex.extra.function.ActionE0;
import cn.xdean.jex.extra.function.FuncE0;

public class IllegalDefineException extends RuntimeException {

  public IllegalDefineException() {
    super();
  }

  public IllegalDefineException(String message, Throwable cause) {
    super(message, cause);
  }

  public IllegalDefineException(String message) {
    super(message);
  }

  public IllegalDefineException(Throwable cause) {
    super(cause);
  }

  public static void assertFalse(boolean b, String message) {
    assertTrue(!b, message);
  }

  public static void assertTrue(boolean b, String message) {
    if (!b) {
      throw new IllegalDefineException(message);
    }
  }

  public static <T> T assertFalse(T t, Predicate<T> test, String message) {
    return assertTrue(t, test.negate(), message);
  }

  public static <T> T assertTrue(T t, Predicate<T> test, String message) {
    assertTrue(test.test(t), message);
    return t;
  }

  public static <T> T assertNonNull(T t, String message) {
    assertTrue(t != null, message);
    return t;
  }

  public static <T> void assertTodo(ActionE0<?> c, String message) {
    try {
      c.call();
    } catch (Exception e) {
      throw new IllegalDefineException(message, e);
    }
  }

  public static <T> T assertTodo(FuncE0<T, ?> c, String message) {
    try {
      return c.call();
    } catch (Exception e) {
      throw new IllegalDefineException(message, e);
    }
  }
}
