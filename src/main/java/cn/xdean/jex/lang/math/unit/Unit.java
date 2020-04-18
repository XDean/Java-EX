package cn.xdean.jex.lang.math.unit;

/**
 * Common Unit Interface
 *
 * @author XDean
 *
 * @param <T> The Unit can convert with. Usually the type itself.
 */
public interface Unit<T extends Unit<T>> {
  /**
   * Convert a long value with given unit to this unit.
   *
   * @param n
   * @param fromUnit
   * @return
   */
  default long convert(long n, T fromUnit) {
    long m = multiple(fromUnit);
    if (m > 0) {
      long max = Long.MAX_VALUE / m;
      if (n > max) {
        return Long.MAX_VALUE;
      } else if (n < -max) {
        return Long.MIN_VALUE;
      } else {
        return n * m;
      }
    } else {
      return n / (-m);
    }
  }

  /**
   * Convert a double value with given unit to this unit.
   *
   * @param n
   * @param fromUnit
   * @return
   */
  default double convert(double n, T fromUnit) {
    long m = multiple(fromUnit);
    if (m > 0) {
      return n * m;
    } else {
      return n / (-m);
    }
  }

  long multiple(T fromUnit);
}
