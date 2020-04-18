package cn.xdean.jex.lang.math;

import cn.xdean.jex.lang.collection.IntList;
import io.reactivex.Flowable;
import org.junit.Test;

public class CartesianProductTest {

  @Test
  public void test() {
    CartesianProduct.cartesianProduct(Flowable.just(
        Flowable.just(0, 1),
        Flowable.just(2, 3),
        Flowable.just(4, 5)
        ))
        .flatMap(is -> Flowable.fromIterable(IntList.create(is).boxed()))
        .test()
        .assertValueCount(24)
        .assertValues(
            0, 2, 4,
            0, 2, 5,
            0, 3, 4,
            0, 3, 5,
            1, 2, 4,
            1, 2, 5,
            1, 3, 4,
            1, 3, 5
        );
  }

  @Test
  public void testEmpty() throws Exception {
    CartesianProduct.cartesianProduct(Flowable.empty())
        .test()
        .assertValueCount(0)
        .assertComplete();
  }
}
