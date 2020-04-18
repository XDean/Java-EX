package cn.xdean.jex.reflect.model;

import xdean.codecov.CodecovIgnore;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Type;

@CodecovIgnore
public final class GenericArrayTypeImpl implements GenericArrayType {
  private final Type componentType;

  public GenericArrayTypeImpl(Type componentType) {
    this.componentType = componentType;
  }

  @Override
  public Type getGenericComponentType() {
    return componentType;
  }

  @Override
  public String toString() {
    Type componentType = getGenericComponentType();
    StringBuilder sb = new StringBuilder();

    if (componentType instanceof Class) {
      sb.append(((Class<?>) componentType).getName());
    } else {
      sb.append(componentType.toString());
    }
    sb.append("[]");
    return sb.toString();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof GenericArrayType) {
      GenericArrayType that = (GenericArrayType) o;

      Type thatComponentType = that.getGenericComponentType();
      return componentType == null ? thatComponentType == null : componentType.equals(thatComponentType);
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return (componentType == null) ? 0 : componentType.hashCode();
  }
}