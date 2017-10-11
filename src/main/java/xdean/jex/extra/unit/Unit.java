package xdean.jex.extra.unit;

public interface Unit<T extends Unit<T>> {
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
