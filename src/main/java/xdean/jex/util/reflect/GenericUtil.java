package xdean.jex.util.reflect;

import static xdean.jex.util.function.Predicates.not;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import lombok.extern.slf4j.Slf4j;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

@Slf4j
public class GenericUtil {
  private static final Type[] EMPTY_TYPE_ARRAY = new Type[0];

  public static ParameterizedType createParameterizedType(Class<?> rawType, Type ownerType, Type... actualTypeArguments) {
    return ParameterizedTypeImpl.make(rawType, actualTypeArguments, ownerType);
  }

  public static WildcardType createWildcardType(Type[] upperBounds, Type[] lowerBounds) {
    Type[] up = nullToEmpty(upperBounds);
    Type[] low = nullToEmpty(lowerBounds);
    return new WildcardType() {
      @Override
      public Type[] getUpperBounds() {
        return up;
      }

      @Override
      public Type[] getLowerBounds() {
        return low;
      }

      @Override
      public int hashCode() {
        return Arrays.hashCode(low) ^ Arrays.hashCode(up);
      }

      @Override
      public boolean equals(Object o) {
        if (o instanceof WildcardType) {
          WildcardType that = (WildcardType) o;
          return Arrays.equals(this.getLowerBounds(), that.getLowerBounds()) && Arrays.equals(this.getUpperBounds(),
              that.getUpperBounds());
        } else {
          return false;
        }
      }
    };
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
   * class IntList extends ArrayList&#60;Integer&#62;{}
   * getGenericType(IntList.class, List.class);// {Integer.class}
   * getGenericType(IntList.class, Collection.class);// {Integer.class}
   * getGenericType(Integer.class, Comparable.class);// {Integer.class}
   * </code>
   * </pre>
   *
   * And nested situation
   *
   * <pre>
   * <code>
   * class A&#60;E,T&#62;{}
   * class B&#60;E&#62; extends A&#60;E,Integer&#62;{}
   * class C extends B&#60;B&#60;Boolean&#62;&#62;{}
   * getGenericType(C.class, A.class);// {B&#60;Boolean&#62;(ParameterizedType), Integer.class}
   * getGenericType(B.class, A.class);// {E(TypeVariable), Integer.class}
   * </code>
   * </pre>
   *
   * @param sourceType The type to find generic type. May Class or ParameterizedType
   * @param targetClass Find the actual generic type on this type.
   * @return A type array. Its length equals targetClass's generic parameters' length. Its elements can be
   *         {@code Class, TypeVariable, ParameterizedType}.
   */
  public static Type[] getGenericTypes(Type sourceType, Class<?> targetClass) {
    TypeVariable<?>[] targetTypeParameters = targetClass.getTypeParameters();
    if (targetTypeParameters.length == 0) {
      return EMPTY_TYPE_ARRAY;
    }
    Map<TypeVariable<?>, Type> map = getGenericReferenceMap(sourceType);
    // If the sourceType is Class, there may left TypeVariable.
    List<TypeVariable<?>> leftTypeParameters = sourceType instanceof Class ?
        Arrays.asList(((Class<?>) sourceType).getTypeParameters()) :
        Collections.emptyList();
    return Arrays.stream(targetTypeParameters)
        .map(tv -> {
          Type actualType = getActualType(map, tv);
          // If actualType equals tv, that means it doesn't implement the targetClass
            return Objects.equals(actualType, tv) && !leftTypeParameters.contains(actualType) ? null : actualType;
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
          Arrays.stream(((ParameterizedType) type).getActualTypeArguments())
              .map(t -> getActualType(map, t))
              .toArray(Type[]::new));
    } else if (type instanceof WildcardType) {
      return createWildcardType(
          Arrays.stream(((WildcardType) type).getUpperBounds())
              .map(t -> getActualType(map, t))
              .toArray(Type[]::new),
          Arrays.stream(((WildcardType) type).getLowerBounds())
              .map(t -> getActualType(map, t))
              .toArray(Type[]::new));
    } else {
      // Class
      return type;
    }
  }

  private static Type[] nullToEmpty(Type[] upperBounds) {
    return upperBounds == null ? EMPTY_TYPE_ARRAY : upperBounds;
  }
}
