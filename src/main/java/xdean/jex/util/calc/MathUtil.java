package xdean.jex.util.calc;

import rx.Observable;
import lombok.experimental.UtilityClass;

@UtilityClass
public class MathUtil {

  public double squareSum(double... doubles) {
    double sum = 0;
    for (double d : doubles) {
      sum += d * d;
    }
    return sum;
  }

  public int squareSum(int... ints) {
    int sum = 0;
    for (int i : ints) {
      sum += i * i;
    }
    return sum;
  }

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

  public static Observable<int[]> cartesianProduct(Observable<Observable<Integer>> sources) {
    return CartesianProduct.cartesianProduct(sources);
  }
}
