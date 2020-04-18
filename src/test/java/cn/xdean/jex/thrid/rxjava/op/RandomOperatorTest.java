package cn.xdean.jex.thrid.rxjava.op;

import io.reactivex.Flowable;
import io.reactivex.Observable;
import org.junit.Test;

public class RandomOperatorTest {
  @Test
  public void test() throws Exception {
    Flowable.range(1, 100)
        .lift(RandomOperator.flowable())
        .test(50)
        .assertValueCount(50);
    Observable.range(1, 100)
        .lift(RandomOperator.observable())
        .test()
        .assertValueCount(100);
  }
}
