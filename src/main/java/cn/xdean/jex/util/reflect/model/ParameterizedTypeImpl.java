package cn.xdean.jex.util.reflect.model;

import java.lang.reflect.MalformedParameterizedTypeException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import java.util.Objects;

import xdean.codecov.CodecovIgnore;

@CodecovIgnore
public final class ParameterizedTypeImpl implements ParameterizedType {
  private final Class<?> rawType;
  private final Type ownerType;
  private final Type[] actualTypeArguments;

  public ParameterizedTypeImpl(Class<?> rawType, Type ownerType, Type[] actualTypeArguments) {
    this.rawType = rawType;
    this.ownerType = ownerType == null ? rawType.getDeclaringClass() : ownerType;
    this.actualTypeArguments = actualTypeArguments;

    TypeVariable<?>[] formals = rawType.getTypeParameters();
    if (formals.length != actualTypeArguments.length) {
      throw new MalformedParameterizedTypeException();
    }
  }

  @Override
  public Type getRawType() {
    return rawType;
  }

  @Override
  public Type getOwnerType() {
    return ownerType;
  }

  @Override
  public Type[] getActualTypeArguments() {
    return actualTypeArguments.clone();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof ParameterizedType) {
      ParameterizedType that = (ParameterizedType) o;
      if (this == that) {
        return true;
      }
      Type thatOwner = that.getOwnerType();
      Type thatRawType = that.getRawType();
      return Objects.equals(ownerType, thatOwner) &&
          Objects.equals(rawType, thatRawType) &&
          Arrays.equals(actualTypeArguments, that.getActualTypeArguments());
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(actualTypeArguments) ^
        (ownerType == null ? 0 : ownerType.hashCode()) ^
        (rawType == null ? 0 : rawType.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (ownerType != null) {
      if (ownerType instanceof Class) {
        sb.append(((Class<?>) ownerType).getName());
      } else {
        sb.append(ownerType.toString());
      }
      sb.append(".");
      if (ownerType instanceof ParameterizedType) {
        sb.append(rawType.getName().replace(
            ((Class<?>) ((ParameterizedType) ownerType).getRawType()).getName() + "$", ""));
      } else {
        sb.append(rawType.getName());
      }
    } else {
      sb.append(rawType.getName());
    }
    if (actualTypeArguments != null && actualTypeArguments.length > 0) {
      sb.append("<");
      boolean first = true;
      for (Type t : actualTypeArguments) {
        if (!first) {
          sb.append(", ");
        }
        if (t instanceof Class) {
          sb.append(((Class<?>) t).getName());
        } else {
          sb.append(t.toString());
        }
        first = false;
      }
      sb.append(">");
    }
    return sb.toString();
  }
}