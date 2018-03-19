package xdean.jex.extra;

import org.junit.Test;

import io.reactivex.Flowable;

public class RelativeComparatorTest {
  @Test
  public void test1() throws Exception {
    Flowable.range(0, 6)
        .sorted(RelativeComparator.of(5, 1, 3))
        .test()
        .assertValues(0, 5, 2, 1, 4, 3);
  }

  @Test
  public void test2() throws Exception {
    Flowable.range(0, 6)
        .sorted(
            RelativeComparator.<Integer> create()
                .addOrder(5, 3)
                .addOrder(2, 4)
                .toComparator()
        )
        .test()
        .assertValues(0, 1, 5, 2, 3, 4);
  }

  @Test
  public void test3() throws Exception {
    Flowable.range(0, 6)
        .sorted(
            RelativeComparator.<Integer> create()
                .addOrder(5, 1)
                .addOrder(1, 3)
                .toComparator()
        )
        .test()
        .assertValues(0, 5, 2, 1, 4, 3);
  }

  @Test
  public void test4() throws Exception {
    Flowable.range(0, 6)
        .sorted(
            RelativeComparator.<Integer> create()
                .addOrder(5, 1)
                .addOrder(5, 3)
                .toComparator()
        )
        .test()
        .assertValues(0, 5, 2, 1, 4, 3);
  }
}
