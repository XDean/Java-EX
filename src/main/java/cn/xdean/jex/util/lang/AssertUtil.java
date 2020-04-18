package cn.xdean.jex.util.lang;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class AssertUtil {

  public static void assertTrue(boolean b) {
    if (!b) {
      throw new AssertionError();
    }
  }

  public static void assertEmpty(Optional<?> o) {
    assertTrue(!o.isPresent());
  }

  public static void assertPresent(Optional<?> o) {
    assertTrue(o.isPresent());
  }

  public static void assertNotNull(Object obj) {
    Objects.requireNonNull(obj, "ASSERT NOTNULL FAIL");
  }

  public static void assertContainsAll(Map<?, ?> map, Object... keys) {
    if (!isAllTrue(map::containsKey, keys)) {
      throw new AssertionError("The map should has following keys: " + Arrays.toString(keys));
    }
  }

  public static void assertContainsAll(List<?> list, Object... elements) {
    if (!isAllTrue(list::contains, elements)) {
      throw new AssertionError("The list should has following elements: " + Arrays.toString(elements));
    }
  }

  @SafeVarargs
  private static <T> boolean isAllTrue(Predicate<? super T> predicate, T... elements) {
    return !Stream.of(elements).filter(predicate.negate()).findFirst().isPresent();
  }

  public static void assertInstanceOf(Object object, Class<?> expectClass) {
    boolean b = expectClass.isInstance(object);
    if (b == false) {
      throw new AssertionError(
          String.format("The expect class is %s, but actually %s.", expectClass.getName(),
              object.getClass().getName()));
    }
  }
}
