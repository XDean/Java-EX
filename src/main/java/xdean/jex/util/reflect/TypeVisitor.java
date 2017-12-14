package xdean.jex.util.reflect;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.function.Function;
import java.util.function.Supplier;

import xdean.jex.util.log.Logable;

public class TypeVisitor<T> implements Logable {

  public static <T> TypeVisitor<T> create(Type type) {
    return new TypeVisitor<>(type);
  }

  /**
   * Because eclipse's type inference is so bad, use this method you can free from angle brackets.
   *
   * @param type
   * @param function
   * @return
   */
  public static <T> T of(Type type, Function<TypeVisitor<T>, T> function) {
    return function.apply(create(type));
  }

  private Type type;
  private T result;
  private boolean visited;

  public TypeVisitor(Type type) {
    super();
    this.type = type;
  }

  @SuppressWarnings("rawtypes")
  public TypeVisitor<T> onClass(Function<Class, T> function) {
    return on(Class.class, function);
  }

  @SuppressWarnings("rawtypes")
  public TypeVisitor<T> onTypeVariable(Function<TypeVariable, T> function) {
    return on(TypeVariable.class, function);
  }

  public TypeVisitor<T> onParameterizedType(Function<ParameterizedType, T> function) {
    return on(ParameterizedType.class, function);
  }

  public TypeVisitor<T> onWildcardType(Function<WildcardType, T> function) {
    return on(WildcardType.class, function);
  }

  public TypeVisitor<T> onGenericArrayType(Function<GenericArrayType, T> function) {
    return on(GenericArrayType.class, function);
  }

  public T result() {
    if (!visited) {
      throw new IllegalStateException("The type haven't been visited: " + type);
    }
    return result;
  }

  public T result(T defaultValue) {
    if (!visited) {
      trace().log("The type haven't been visited: " + type);
      return defaultValue;
    }
    return result;
  }

  public T result(Supplier<T> defaultValueSupplier) {
    if (!visited) {
      trace().log("The type haven't been visited: " + type);
      return defaultValueSupplier.get();
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  public <K extends Type> TypeVisitor<T> on(Class<K> klass, Function<K, T> action) {
    if (!visited && klass.isInstance(type)) {
      visited = true;
      result = action.apply((K) type);
    }
    return this;
  }
}
