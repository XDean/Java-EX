package xdean.jex.util.reflect;

import static xdean.jex.util.function.Predicates.not;

import java.lang.reflect.GenericArrayType;
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

import xdean.jex.util.reflect.model.GenericArrayTypeImpl;
import xdean.jex.util.reflect.model.ParameterizedTypeImpl;
import xdean.jex.util.reflect.model.WildcardTypeImpl;

public class GenericUtil {

  private static final Type[] EMPTY_TYPE_ARRAY = new Type[0];

  public static ParameterizedType createParameterizedType(Class<?> rawType, Type ownerType,
      Type... actualTypeArguments) {
    return new ParameterizedTypeImpl(rawType, ownerType, actualTypeArguments);
  }

  public static WildcardType createWildcardType(Type[] upperBounds, Type[] lowerBounds) {
    return new WildcardTypeImpl(nullToEmpty(lowerBounds), nullToEmpty(upperBounds));
  }

  public static GenericArrayType createGenericArrayType(Type componentType) {
    return new GenericArrayTypeImpl(componentType);
  }

  public static Map<TypeVariable<?>, Type> getGenericReferenceMap(Type type) {
    return TypeVisitor.of(type, b -> b
        .onClass(GenericUtil::getGenericReferenceMap)
        .onParameterizedType(GenericUtil::getGenericReferenceMap)
        .result(Collections::emptyMap));
  }

  public static Map<TypeVariable<?>, Type> getGenericReferenceMap(ParameterizedType parameterizedType) {
    return TypeVisitor.of(parameterizedType.getRawType(), b -> b
        .onClass(c -> {
          HashMap<TypeVariable<?>, Type> map = new HashMap<>();
          map.putAll(getGenericReferenceMap(c));
          Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
          TypeVariable<?>[] implTypeParams = (c).getTypeParameters();
          for (int i = 0; i < actualTypeArguments.length; i++) {
            map.put(implTypeParams[i], actualTypeArguments[i]);
          }
          return map;
        })
        .result(Collections::emptyMap));
  }

  public static Map<TypeVariable<?>, Type> getGenericReferenceMap(Class<?> clz) {
    HashMap<TypeVariable<?>, Type> map = new HashMap<>();
    List<Type> allTypes = new ArrayList<>();
    allTypes.add(clz.getGenericSuperclass());
    allTypes.addAll(Arrays.asList(clz.getGenericInterfaces()));
    allTypes.stream()
        .filter(not(null))
        .map(GenericUtil::getGenericReferenceMap)
        .forEach(map::putAll);
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
   * class D&#60;T&#62; extends B&#60;B&#60;? extends T&#62;&#62;{}
   * class E extends D&#60;Number&#62;{}
   * getGenericType(B.class, A.class);// {E(TypeVariable), Integer.class}
   * getGenericType(C.class, A.class);// {B&#60;Boolean&#62;(ParameterizedType), Integer.class}
   * getGenericType(E.class, A.class);// {B&#60;? extends Number&#62;(ParameterizedType), Integer.class}
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
    List<TypeVariable<?>> leftTypeParameters = sourceType instanceof Class
        ? Arrays.asList(((Class<?>) sourceType).getTypeParameters())
        : Collections.emptyList();
    return Arrays.stream(targetTypeParameters)
        .map(tv -> {
          Type actualType = getActualType(map, tv);
          // If actualType equals tv, that means it doesn't implement the targetClass
          return Objects.equals(actualType, tv) && !leftTypeParameters.contains(actualType) ? null : actualType;
        })
        .toArray(Type[]::new);
  }

  /**
   * Get actual type by the type map.
   *
   * @param map
   * @param type
   * @return The actual type. Return itself if it's already the most explicit type.
   */
  private static Type getActualType(Map<TypeVariable<?>, Type> map, Type type) {
    return TypeVisitor.of(type, b -> b
        .onClass(c -> c)
        .onTypeVariable(tv -> map.containsKey(tv) ? getActualType(map, map.get(tv)) : tv)
        .onParameterizedType(pt -> TypeVisitor.of(pt.getRawType(), bb -> bb
            .onClass(c -> createParameterizedType(c, pt.getOwnerType(),
                Arrays.stream(pt.getActualTypeArguments()).map(t -> getActualType(map, t)).toArray(Type[]::new)))
            .result()))
        .onWildcardType(wt -> createWildcardType(
            Arrays.stream(wt.getUpperBounds()).map(t -> getActualType(map, t)).toArray(Type[]::new),
            Arrays.stream(wt.getLowerBounds()).map(t -> getActualType(map, t)).toArray(Type[]::new)))
        .onGenericArrayType(gat -> createGenericArrayType(getActualType(map, gat.getGenericComponentType())))
        .result(type));
  }

  private static Type[] nullToEmpty(Type[] upperBounds) {
    return upperBounds == null ? EMPTY_TYPE_ARRAY : upperBounds;
  }
}
