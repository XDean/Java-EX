package xdean.jex.util.calc;

import lombok.experimental.UtilityClass;

@UtilityClass
public class MathUtil {
  public double toRange(double value, double min, double max) {
    return Math.max(Math.min(max, value), min);
  }

  public int toRange(int value, int min, int max) {
    return Math.max(Math.min(max, value), min);
  }

  public boolean inRange(double value, double min, double max) {
    return toRange(value, min, max) == value;
  }

  public boolean inRange(int value, int min, int max) {
    return toRange(value, min, max) == value;
  }
}
