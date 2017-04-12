package xdean.jex.extra.rx.op;

import org.junit.Test;

import rx.Observable;

public class TestFunctionOperator {

  @Test
  public void test() {
    Observable.range(0, 5)
        .lift(FunctionOperator.of(this::func))
        .test()
        .assertResult(0, -1, -2, -3, -4);
  }

  private Observable<Integer> func(Observable<Integer> ob) {
    return ob.map(i -> -i);
  }
}
