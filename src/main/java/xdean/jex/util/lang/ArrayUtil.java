package xdean.jex.util.lang;

import static xdean.jex.util.lang.PrimitiveTypeUtil.*;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.function.Consumer;

public class ArrayUtil {

  public static <T> void forEach(T[] array, Consumer<? super T> action) {
    Arrays.stream(array).forEach(action);
  }

  public static void forEach(Object array, Consumer<Object> action) {
    if (!array.getClass().isArray()) {
      throw new IllegalArgumentException("The parameter is not array.");
    }
    int len = Array.getLength(array);
    for (int i = 0; i < len; i++) {
      action.accept(Array.get(array, i));
    }
  }

  public static Object deepClone(Object array) {
    if (array == null || array.getClass().isArray() == false) {
      return array;
    }
    int len = Array.getLength(array);
    Object newArray = Array.newInstance(array.getClass().getComponentType(), len);
    for (int i = 0; i < len; i++) {
      Array.set(newArray, i, deepClone(Array.get(array, i)));
    }
    return newArray;
  }

  public static int[][] transpose(int[][] origin) {
    return (int[][]) toPrimitiveArray(transpose((Integer[][]) toWrapperArray(origin)));
  }

  public static long[][] transpose(long[][] origin) {
    return (long[][]) toPrimitiveArray(transpose((Long[][]) toWrapperArray(origin)));
  }

  @SuppressWarnings("unchecked")
  public static <T> T[][] transpose(T[][] origin) {
    Class<? extends T[][]> clz = (Class<? extends T[][]>) origin.getClass();
    T[][] ret = (T[][]) Array.newInstance(clz.getComponentType(), origin[0].length);
    int created = 0;
    int len = 0;
    for (int i = origin.length - 1; i >= 0; i--) {
      if (origin[i].length < len) {
        throw new IllegalArgumentException();
      }
      len = origin[i].length;
      for (int m = created; m < len; m++) {
        ret[m] = (T[]) Array.newInstance(clz.getComponentType().getComponentType(), i + 1);
      }
      created = len;
      for (int m = 0; m < origin[i].length; m++) {
        ret[m][i] = origin[i][m];
      }
    }
    return ret;
  }

  public static int compare(int[] a, int[] b) {
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

  public static <T extends Comparable<T>> int compare(T[] a, T[] b) {
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
