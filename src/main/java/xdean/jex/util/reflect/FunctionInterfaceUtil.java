package xdean.jex.util.reflect;

import static xdean.jex.util.lang.PrimitiveTypeUtil.toWrapper;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import xdean.jex.util.log.Log;
import xdean.jex.util.log.LogUtil;

public class FunctionInterfaceUtil {

  private static final Log LOG = LogUtil.log();

  /**
   * Get the method from a function interface
   *
   * @param clz
   * @return null if the given class is not a function interface
   */
  public static <T> Method getFunctionInterfaceMethod(Class<?> clz) {
    if (!clz.isInterface()) {
      return null;
    }
    Method[] ms = Stream.of(clz.getMethods())
        .filter(m -> !(m.isDefault() || m.isBridge() || m.isSynthetic() || Modifier.isStatic(m.getModifiers())))
        .toArray(Method[]::new);
    if (ms.length != 1) {
      return null;
    }
    return ms[0];
  }

  /**
   * Adapt a method to a function interface.<br>
   * For example:
   *
   * <pre>
   * <code>
   * static int increment(int i){
   *  return i+1;
   * }
   * Method m = ...
   * UnaryOperator&#60;Integer&#62; uo = methodToFunctionInterface(m, null, UnaryOperator.class);//work
   * UnaryOperator&#60;Integer&#62; uo = methodToFunctionInterface(m, null, UnaryOperator.class, Integer.class);//work and more safe
   * UnaryOperator&#60;Integer&#62; uo = methodToFunctionInterface(m, null, UnaryOperator.class, String.class);//return null
   * </code>
   * </pre>
   *
   * Note that only indeed adapted method can be converted. <br>
   * For example: {@code Integer function(Number)} can't adapt to {@code UnaryOperator<Integer>}, though any call to a
   * {@code UnaryOperator<Integer>} can delegate by the function. Because you can't define like:
   *
   * <pre>
   * <code>
   * interface Function extends UnaryOperator&#60;Integer&#62; {
   *   &#64;Override
   *   public Integer apply(Number t);//error
   * }
   * </code>
   * </pre>
   *
   * Now only ignored things is ParameterizedType. That means this method consider return type and parameter types as
   * raw type.
   *
   * @param method The method to adapt. Ensure the method can be access.
   * @param target The method's target. If the method is static, target should be null.
   * @param functionInterfaceClass The function interface to adapt to.
   * @param explicitGenericTypes If the function interface has generic type, you can specify them in order. If a
   *          explicit type is null, it will be ignored.
   * @return Instance of the function interface. Or null if can't adapt to. Note that returned object is raw type. If
   *         you don't specify explicit generic types, IllegalArgumentException(type mismatch) may happen when you call
   *         it.
   */
  @SuppressWarnings("unchecked")
  public static <T> T methodToFunctionInterface(Method method, Object target, Class<T> functionInterfaceClass,
      Class<?>... explicitGenericTypes) {
    Method functionMethod = getFunctionInterfaceMethod(functionInterfaceClass);
    if (functionMethod == null) {
      return null;
    }
    if (functionMethod.getParameterCount() != method.getParameterCount()) {
      return null;
    }
    // Map the explicit type
    Map<TypeVariable<?>, Class<?>> explicitTypeMap = new HashMap<>();
    TypeVariable<Class<T>>[] typeParameters = functionInterfaceClass.getTypeParameters();
    if (explicitGenericTypes.length > typeParameters.length) {
      throw new IllegalArgumentException("The explicit generic types are too many. Expect " + typeParameters.length);
    }
    for (int i = 0; i < explicitGenericTypes.length; i++) {
      explicitTypeMap.put(typeParameters[i], toWrapper(explicitGenericTypes[i]));
    }
    // Map the generic reference
    Map<TypeVariable<?>, Type> typeVariableReference = GenericUtil.getGenericReferenceMap(functionInterfaceClass);
    Function<TypeVariable<?>, Class<?>> getActualTypeVariable = tv -> {
      Type next;
      while (true) {
        next = typeVariableReference.get(tv);
        if (next == null) {
          return explicitTypeMap.get(tv);
        }
        if (next instanceof Class<?>) {
          return (Class<?>) next;
        }
        tv = (TypeVariable<?>) next;
      }
    };
    // Resolve return type
    Class<?> returnType = toWrapper(method.getReturnType());
    Type functionGenericReturnType = functionMethod.getGenericReturnType();
    if (functionGenericReturnType instanceof ParameterizedType) {
      // TODO handle and match ParameterizedType
      functionGenericReturnType = ((ParameterizedType) functionGenericReturnType).getRawType();
    }
    if (returnType == void.class && functionGenericReturnType == void.class) {
    } else if (functionGenericReturnType instanceof Class) {
      if (!toWrapper((Class<?>) functionGenericReturnType).isAssignableFrom(returnType)) {
        return null;
      }
    } else if (functionGenericReturnType instanceof TypeVariable) {
      TypeVariable<?> tv = (TypeVariable<?>) functionGenericReturnType;
      Class<?> explicitType = getActualTypeVariable.apply(tv);
      if (explicitType != null) {
        if (!explicitType.equals(returnType)) {
          return null;
        }
      } else if (FunctionInterfaceUtil.matchTypeBounds(returnType, tv)) {
        explicitTypeMap.put(tv, returnType);
      } else {
        return null;
      }
    } else {
      LOG.warning().log("Can't handle GenericReturnType: {} with type {}", functionGenericReturnType,
          functionGenericReturnType.getClass());
      return null;
    }
    // Resolve parameters
    Type[] functionParams = functionMethod.getGenericParameterTypes();
    Class<?>[] params = method.getParameterTypes();
    for (int i = 0; i < params.length; i++) {
      Type functionParamType = functionParams[i];
      Class<?> paramType = toWrapper(params[i]);
      if (functionParamType instanceof ParameterizedType) {
        // TODO handle and match ParameterizedType
        functionParamType = ((ParameterizedType) functionParamType).getRawType();
      }
      if (functionParamType instanceof Class) {
        if (!paramType.isAssignableFrom(
            toWrapper((Class<?>) functionParamType))) {
          return null;
        }
      } else if (functionParamType instanceof TypeVariable) {
        TypeVariable<?> tv = (TypeVariable<?>) functionParamType;
        Class<?> explicitType = getActualTypeVariable.apply(tv);
        if (explicitType != null) {
          if (!explicitType.equals(paramType)) {
            return null;
          }
        } else if (FunctionInterfaceUtil.matchTypeBounds(paramType, tv)) {
          explicitTypeMap.put(tv, paramType);
        } else {
          return null;
        }
      } else {
        LOG.warning().log("Can't handle GenericParameterType: {} with type {}", paramType, paramType.getClass());
        return null;
      }
    }
    // Resolve throws
    List<Type> functionExceptionTypes = Arrays.asList(functionMethod.getGenericExceptionTypes());
    for (Class<?> exceptionType : method.getExceptionTypes()) {
      if (Exception.class.isAssignableFrom(exceptionType)
          && !RuntimeException.class.isAssignableFrom(exceptionType)
          && !functionExceptionTypes.stream().anyMatch(
              functionThrowType -> {
                Class<?> functionThrowClass = null;
                if (functionThrowType instanceof Class) {
                  functionThrowClass = (Class<?>) functionThrowType;
                } else if (functionThrowType instanceof TypeVariable) {
                  Class<?> explicitType = explicitTypeMap.get(functionThrowType);
                  if (explicitType == null) {
                    return FunctionInterfaceUtil.matchTypeBounds(exceptionType, (TypeVariable<?>) functionThrowType);
                  } else {
                    functionThrowClass = explicitType;
                  }
                } else {
                  LOG.warning().log("Can't handle GenericException: {} with type {}", functionThrowType,
                      functionThrowType.getClass());
                  return false;
                }
                return functionThrowClass.isAssignableFrom(exceptionType);
              })) {
        return null;
      }
    }
    return (T) Proxy.newProxyInstance(
        functionInterfaceClass.getClassLoader(),
        new Class[] { functionInterfaceClass },
        (obj, m, args) -> {
          if (m.equals(functionMethod)) {
            return method.invoke(target, args);
          }
          Class<?> declaringClass = m.getDeclaringClass();
          if (m.isDefault() || declaringClass.equals(Object.class)) {
            Object result;
            Constructor<MethodHandles.Lookup> constructor = MethodHandles.Lookup.class.getDeclaredConstructor(
                Class.class, int.class);
            constructor.setAccessible(true);
            result = constructor
                .newInstance(declaringClass, MethodHandles.Lookup.PRIVATE)
                .unreflectSpecial(m, declaringClass)
                .bindTo(obj)
                .invokeWithArguments(args);
            return (result);
          }
          return m.invoke(obj, args);
        });
  }

  private static boolean matchTypeBounds(Class<?> clz, TypeVariable<?> tv) {
    Class<?> wrapClz = toWrapper(clz);
    return FunctionInterfaceUtil.getAllBounds(tv).allMatch(
        c -> toWrapper(c).isAssignableFrom(wrapClz));
  }

  private static Stream<Class<?>> getAllBounds(TypeVariable<?> tv) {
    return Stream.of(tv.getBounds())
        .flatMap(t -> {
          if (t instanceof Class) {
            return Stream.of((Class<?>) t);
          } else if (t instanceof TypeVariable) {
            return getAllBounds(((TypeVariable<?>) t));
          } else {
            LOG.warning().log("Can't handle TypeVariable Bound: {} with type {}", t, t.getClass());
            return Stream.empty();
          }
        });
  }

  // private static Map<TypeVariable<?>, Type> getTypeVariableReference(Class<?> clz) {
  // HashMap<TypeVariable<?>, Type> map = new HashMap<>();
  // if (clz.getSuperclass() != null) {
  // map.putAll(getTypeVariableReference(clz.getSuperclass()));
  // }
  // Arrays.asList(clz.getInterfaces()).forEach(c -> map.putAll(getTypeVariableReference(c)));
  // Stream.concat(Stream.of(clz.getGenericSuperclass()), Stream.of(clz.getGenericInterfaces()))
  // .filter(not(null))
  // .forEach(c -> {
  // if (c instanceof Class) {
  // } else if (c instanceof ParameterizedType) {
  // Type[] actualTypeArguments = ((ParameterizedType) c).getActualTypeArguments();
  // TypeVariable<?>[] implTypeParams = ((Class<?>) ((ParameterizedType) c).getRawType())
  // .getTypeParameters();
  // for (int i = 0; i < actualTypeArguments.length; i++) {
  // map.put(implTypeParams[i], actualTypeArguments[i]);
  // }
  // } else {
  // log.warn("Unknown Generic Type: {} with type {}", c, c.getClass());
  // }
  // });
  // return map;
  // }
}
