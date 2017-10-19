package xdean.jex.util.reflect;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;

import xdean.jex.extra.function.FunctionThrow;

public class TypeVisitor<T> {

  public static <T> TypeVisitor<T> create(Type type) {
    return new TypeVisitor<>(type);
  }

  private Type type;
  private T result;
  private boolean visited;

  public TypeVisitor(Type type) {
    super();
    this.type = type;
  }

  @SuppressWarnings("rawtypes")
  public <E extends Exception> TypeVisitor<T> onClass(FunctionThrow<Class, T, E> function) throws E {
    return on(Class.class, function);
  }

  @SuppressWarnings("rawtypes")
  public <E extends Exception> TypeVisitor<T> onTypeVariable(FunctionThrow<TypeVariable, T, E> function) throws E {
    return on(TypeVariable.class, function);
  }

  public <E extends Exception> TypeVisitor<T> onParameterizedType(FunctionThrow<ParameterizedType, T, E> function)
      throws E {
    return on(ParameterizedType.class, function);
  }

  public <E extends Exception> TypeVisitor<T> onWildcardType(FunctionThrow<WildcardType, T, E> function)
      throws E {
    return on(WildcardType.class, function);
  }

  public <E extends Exception> TypeVisitor<T> onGenericArrayType(FunctionThrow<GenericArrayType, T, E> function)
      throws E {
    return on(GenericArrayType.class, function);
  }

  public T result() {
    if (!visited) {
      throw new IllegalStateException("The type haven't been visited: " + type);
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  public <K extends Type, E extends Exception> TypeVisitor<T> on(Class<K> klass, FunctionThrow<K, T, E> action)
      throws E {
    if (!visited && klass.isInstance(type)) {
      visited = true;
      result = action.apply((K) type);
    }
    return this;
  }
}
