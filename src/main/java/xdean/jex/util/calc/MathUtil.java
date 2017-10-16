package xdean.jex.util.calc;


public class MathUtil {

  public static double squareSum(double... doubles) {
    double sum = 0;
    for (double d : doubles) {
      sum += d * d;
    }
    return sum;
  }

  public static int squareSum(int... ints) {
    int sum = 0;
    for (int i : ints) {
      sum += i * i;
    }
    return sum;
  }

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
