package xdean.jex.util.lang;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ArrayUtil {
  public int compare(int[] a, int[] b) {
    if (a.length != b.length) {
      throw new IllegalArgumentException("Can't compare different length arrays");
    }
    for (int i = 0; i < a.length; i++) {
      if (a[i] == b[i]) {
        continue;
      } else {
        return a[i] - b[i];
      }
    }
    return 0;
  }

  public <T extends Comparable<T>> int compare(T[] a, T[] b) {
    if (a.length != b.length) {
      throw new IllegalArgumentException("Can't compare different length arrays");
    }
    for (int i = 0; i < a.length; i++) {
      int compare = a[i].compareTo(b[i]);
      if (compare == 0) {
        continue;
      } else {
        return compare;
      }
    }
    return 0;
  }
}
