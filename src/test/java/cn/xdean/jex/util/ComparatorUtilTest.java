package cn.xdean.jex.util;

import static cn.xdean.jex.util.ComparatorUtil.*;
import io.reactivex.Flowable;

import org.junit.Test;

public class ComparatorUtilTest {
  @Test
  public void testStartWith() throws Exception {
    Flowable.range(0, 10)
        .sorted(startWith(3, 5, 7))
        .take(5)
        .test()
        .assertResult(3, 5, 7, 0, 1);
  }

  @Test
  public void testEndWith() throws Exception {
    Flowable.range(0, 10)
        .sorted(endWith(3, 5, 7))
        .takeLast(5)
        .test()
        .assertResult(8, 9, 3, 5, 7);
  }

  @Test
  public void testRelative() throws Exception {
    Flowable.range(0, 10)
        .sorted(relative(9, 1, 5))
        .test()
        .assertValueAt(1, 9)
        .assertValueAt(5, 1)
        .assertValueAt(9, 5);
  }

  @Test
  public void testComplex() throws Exception {
    Flowable.range(0, 10)
        .sorted(startWith(9, 8))
        .sorted(endWith(1, 2))
        .sorted(startWith(3, 6))
        .sorted(endWith(9, 1))
        .test()
        .assertResult(3, 6, 8, 0, 4, 5, 7, 2, 9, 1);
  }
}
