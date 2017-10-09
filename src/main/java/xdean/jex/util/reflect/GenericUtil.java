package xdean.jex.util.reflect;

import static xdean.jex.util.function.Predicates.not;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class GenericUtil {
  public static Map<TypeVariable<?>, Type> getGenericReferenceMap(Type type) {
    if (type instanceof Class) {
      return getGenericReferenceMap((Class<?>) type);
    } else if (type instanceof ParameterizedType) {
      return getGenericReferenceMap((ParameterizedType) type);
    } else {
      log.warn("Unknown Generic Type: {} with type {}", type, type.getClass());
    }
    return null;
  }

  public static Map<TypeVariable<?>, Type> getGenericReferenceMap(ParameterizedType parameterizedType) {
    HashMap<TypeVariable<?>, Type> map = new HashMap<>();
    Type rawType = parameterizedType.getRawType();
    if (rawType instanceof Class) {
      Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
      TypeVariable<?>[] implTypeParams = ((Class<?>) rawType).getTypeParameters();
      for (int i = 0; i < actualTypeArguments.length; i++) {
        map.put(implTypeParams[i], actualTypeArguments[i]);
      }
    }
    return map;
  }

  public static Map<TypeVariable<?>, Type> getGenericReferenceMap(Class<?> clz) {
    HashMap<TypeVariable<?>, Type> map = new HashMap<>();
    List<Type> allType = new ArrayList<>();
    allType.add(clz.getSuperclass());
    allType.addAll(Arrays.asList(clz.getInterfaces()));
    allType.add(clz.getGenericSuperclass());
    allType.addAll(Arrays.asList(clz.getGenericInterfaces()));
    allType.stream()
        .filter(not(null))
        .forEach(c -> map.putAll(getGenericReferenceMap(c)));
    return map;
  }
}
