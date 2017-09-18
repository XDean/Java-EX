package xdean.jex.util;

import static xdean.jex.util.ComparatorUtil.*;
import io.reactivex.Flowable;

import org.junit.Test;

public class TestComparatorUtil {
  @Test
  public void testSimple() throws Exception {
    Flowable.range(1, 10)
        .sorted(startWith(3, 5, 7))
        .take(5)
        .test()
        .assertResult(3, 5, 7, 1, 2);
    Flowable.range(1, 10)
        .sorted(endWith(3, 5, 7))
        .takeLast(5)
        .test()
        .assertResult(9, 10, 3, 5, 7);
  }

  @Test
  public void testComplex() throws Exception {
    Flowable.range(1, 10)
        .sorted(startWith(9, 8))
        .sorted(endWith(1, 2))
        .sorted(startWith(3, 6))
        .sorted(endWith(9, 1))
        .test()
        .assertResult(3, 6, 8, 4, 5, 7, 10, 2, 9, 1);
  }
}
