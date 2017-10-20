package xdean.jex.util.reflect.model;

import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.util.Arrays;

import xdean.jex.internal.codecov.CodecovIgnore;

@CodecovIgnore
public final class WildcardTypeImpl implements WildcardType {
  private final Type[] lower;
  private final Type[] upper;

  public WildcardTypeImpl(Type[] lower, Type[] upper) {
    this.lower = lower;
    this.upper = upper;
  }

  @Override
  public Type[] getUpperBounds() {
    return upper;
  }

  @Override
  public Type[] getLowerBounds() {
    return lower;
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(lower) ^ Arrays.hashCode(upper);
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

  @Override
  public String toString() {
    Type[] lowerBounds = getLowerBounds();
    Type[] bounds = lowerBounds;
    StringBuilder sb = new StringBuilder();

    if (lowerBounds.length > 0) {
      sb.append("? super ");
    } else {
      Type[] upperBounds = getUpperBounds();
      if (upperBounds.length > 0 && !upperBounds[0].equals(Object.class)) {
        bounds = upperBounds;
        sb.append("? extends ");
      } else {
        return "?";
      }
    }

    assert bounds.length > 0;

    boolean first = true;
    for (Type bound : bounds) {
      if (!first) {
        sb.append(" & ");
      }

      first = false;
      if (bound instanceof Class) {
        sb.append(((Class<?>) bound).getName());
      } else {
        sb.append(bound.toString());
      }
    }
    return sb.toString();
  }
}