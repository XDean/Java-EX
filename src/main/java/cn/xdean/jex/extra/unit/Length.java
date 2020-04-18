package cn.xdean.jex.extra.unit;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.LOCAL_VARIABLE;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.ElementType.TYPE_USE;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Enum of length unit, from meter to femtometer.
 *
 * @author XDean
 *
 */
public enum Length implements Unit<Length> {
  FM(0),
  PM(1),
  NM(2),
  UM(3),
  MM(4),
  M(5);

  /**
   * Annotation to mark the length value is which unit.
   *
   * @author XDean
   */
  @Retention(RetentionPolicy.SOURCE)
  @Target({ METHOD, FIELD, PARAMETER, TYPE, TYPE_USE, LOCAL_VARIABLE })
  public @interface Unit {
    Length value();
  }

  private int pow1000;

  private Length(int pow1000) {
    this.pow1000 = pow1000;
  }

  public long toFM(long value) {
    return FM.convert(value, this);
  }

  public long toPM(long value) {
    return PM.convert(value, this);
  }

  public long toNM(long value) {
    return NM.convert(value, this);
  }

  public long toUM(long value) {
    return UM.convert(value, this);
  }

  public long toMM(long value) {
    return MM.convert(value, this);
  }

  public long toM(long value) {
    return M.convert(value, this);
  }

  public double toFM(double value) {
    return FM.convert(value, this);
  }

  public double toPM(double value) {
    return PM.convert(value, this);
  }

  public double toNM(double value) {
    return NM.convert(value, this);
  }

  public double toUM(double value) {
    return UM.convert(value, this);
  }

  public double toMM(double value) {
    return MM.convert(value, this);
  }

  public double toM(double value) {
    return M.convert(value, this);
  }

  @Override
  public long multiple(Length fromUnit) {
    long multiple = 1;
    int delta = fromUnit.pow1000 - this.pow1000;
    if (delta < 0) {
      multiple = -multiple;
      delta = -delta;
    }
    while (delta > 0) {
      multiple *= 1000;
      delta--;
    }
    return multiple;
  }
}
