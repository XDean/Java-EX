package xdean.jex.util.reflect;

import static xdean.jex.util.function.FunctionAdapter.function;
import static xdean.jex.util.lang.ExceptionUtil.uncheck;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import xdean.jex.util.lang.PrimitiveTypeUtil;

public class ReferenceUtil {
  public static boolean isReferencing(Object from, Object to) {
    Objects.requireNonNull(to);
    return isReferencing(from, to, new IdentityHashMap<>());
  }

  private static boolean isReferencing(Object from, Object to, Map<Object, ?> visited) {
    if (from == to) {
      return true;
    } else if (from == null || visited.containsKey(from)) {
      return false;
    }
    visited.put(from, null);
    Class<? extends Object> clz = from.getClass();
    if (clz.isArray()) {
      if (PrimitiveTypeUtil.isPrimitive(clz.getComponentType())) {
        return false;
      } else {
        return Stream.of((Object[]) from).anyMatch(o -> isReferencing(o, to, visited));
      }
    }
    return Stream.of(ReflectUtil.getAllFields(clz, false))
        .filter(f -> !PrimitiveTypeUtil.isPrimitive(f.getType()))
        .map(function(f -> f.setAccessible(true)))
        .anyMatch(f -> isReferencing(uncheck(() -> f.get(from)), to, visited));
  }
}
