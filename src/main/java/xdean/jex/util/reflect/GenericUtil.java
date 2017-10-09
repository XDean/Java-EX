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
import java.util.Objects;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

@Slf4j
public class GenericUtil {

  public static ParameterizedType createParameterizedType(Class<?> rawType, Type ownerType, Type... actualTypeArguments) {
    return ParameterizedTypeImpl.make(rawType, actualTypeArguments, ownerType);
  }

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
      map.putAll(getGenericReferenceMap((Class<?>) rawType));
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
    List<Type> allTypes = new ArrayList<>();
    allTypes.add(clz.getGenericSuperclass());
    allTypes.addAll(Arrays.asList(clz.getGenericInterfaces()));
    allTypes.stream()
        .filter(not(null))
        .forEach(c -> map.putAll(getGenericReferenceMap(c)));
    return map;
  }

  /**
   * Get the actual generic types.<br>
   * For example:
   *
   * <pre>
   * <code>
   * class IntList extends ArrayList<Integer>{}
   * getGenericType(IntList.class, List.class);// {Integer.class}
   * getGenericType(IntList.class, Collection.class);// {Integer.class}
   * getGenericType(Integer.class, Comparable.class);// {Integer.class}
   * </code>
   * </pre>
   *
   * And even nested situation
   *
   * <pre>
   * <code>
   * class A<E,T>{}
   * class B<E> extends A<E,Integer>{}
   * class C extends<Boolean>{}
   * getGenericType(C.class, A.class);// {Boolean.class, Integer.class}
   * getGenericType(B.class, A.class);// {null, Integer.class}
   * </code>
   * </pre>
   *
   * @param sourceType The type to find generic type.
   * @param targetClass Find the actual generic type on this type.
   * @return An array with Class object. Its length equals targetClass's generic parameters' length. If a generic
   *         parameter is still generic, it will be null in the returned array.
   */
  public static Class<?>[] getGenericClasses(Type sourceType, Class<?> targetClass) {
    return Arrays.asList(getGenericTypes(sourceType, targetClass))
        .stream()
        .map(t -> {
          if (t instanceof Class) {
            return (Class<?>) t;
          } else if (t instanceof ParameterizedType) {
            return (Class<?>) ((ParameterizedType) t).getRawType();
          } else {
            return null;
          }
        })
        .toArray(Class<?>[]::new);
  }

  public static Type[] getGenericTypes(Type sourceType, Class<?> targetClass) {
    Map<TypeVariable<?>, Type> map = getGenericReferenceMap(sourceType);
    TypeVariable<?>[] targetTypeParameters = targetClass.getTypeParameters();
    log.debug("Type paramter length: {}", targetTypeParameters.length);
    if (targetTypeParameters.length == 0) {
      return new Class<?>[0];
    }
    return Stream.of(targetTypeParameters)
        .map(tv -> {
          Type actualType = getActualType(map, tv);
          // If actualType equals tv, that means it doesn't implement the targetClass
            return Objects.equals(actualType, tv) ? null : actualType;
          })
        .toArray(Type[]::new);
  }

  private static Type getActualType(Map<TypeVariable<?>, Type> map, Type type) {
    if (type instanceof TypeVariable) {
      Type mapped = map.get(type);
      return mapped == null ? type : getActualType(map, mapped);
    } else if (type instanceof ParameterizedType) {
      return createParameterizedType((Class<?>) ((ParameterizedType) type).getRawType(),
          ((ParameterizedType) type).getOwnerType(),
          Arrays.asList(((ParameterizedType) type).getActualTypeArguments())
              .stream()
              .map(t -> getActualType(map, t))
              .toArray(Type[]::new));
    } else {
      // Class or wildcard
      return type;
    }
  }
}
