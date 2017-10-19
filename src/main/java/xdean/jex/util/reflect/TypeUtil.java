package xdean.jex.util.reflect;

import java.lang.reflect.Type;
import java.util.Arrays;

public class TypeUtil {
  /**
   * Determine if a 'from' type object can assign to 'to' type.<br>
   * If the to class is primitive type, see {@link Class#isAssignableFrom(Class)}
   *
   * @see Class#isAssignableFrom(Class)
   * @param from
   * @param to
   * @return
   */
  public static boolean isAssignableFrom(Type from, Class<?> to) {
    return TypeVisitor.<Boolean> create(from)
        .onClass(to::isAssignableFrom)
        .onParameterizedType(pt -> TypeVisitor.<Boolean> create(pt.getRawType())
            .onClass(to::isAssignableFrom)
            .result())
        .onTypeVariable(tv -> isAssignableFrom(tv.getBounds(), to))
        .onWildcardType(tv -> isAssignableFrom(tv.getUpperBounds(), to))
        .result();
  }

  private static boolean isAssignableFrom(Type[] fromBounds, Class<?> to) {
    return Arrays.stream(fromBounds).anyMatch(t -> isAssignableFrom(t, to));
  }
}
