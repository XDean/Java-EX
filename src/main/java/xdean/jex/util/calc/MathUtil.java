package xdean.jex.util.calc;

public class MathUtil {
  public static double toRange(double value, double min, double max) {
    return Math.max(Math.min(max, value), min);
  }

  public static int toRange(int value, int min, int max) {
    return Math.max(Math.min(max, value), min);
  }

  public static boolean inRange(double value, double min, double max) {
    return toRange(value, min, max) == value;
  }

  public static boolean inRange(int value, int min, int max) {
    return toRange(value, min, max) == value;
  }
}
