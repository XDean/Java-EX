package xdean.jex.util.calc;

import java.util.Arrays;

import org.junit.Test;

import rx.Observable;

public class CartesianProductTest {

  @Test
  public void test() {
    CartesianProduct.cartesianProduct(Observable.just(
        Observable.just(0, 1),
        Observable.just(2, 3),
        Observable.just(4, 5)
        ))
        .doOnNext(a -> System.out.println(Arrays.toString(a)))
        .test()
        .assertValueCount(8);
  }
}
