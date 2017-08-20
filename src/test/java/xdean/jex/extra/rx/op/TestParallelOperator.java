package xdean.jex.extra.rx.op;

import rx.Observable;
import rx.schedulers.Schedulers;
import xdean.jex.util.lang.ExceptionUtil;

public class TestParallelOperator {

  // @Test
  public void test() {
    Observable.range(1, 100)
        .lift(new ParallelOperator<>(Schedulers.computation()))
        .doOnNext(e -> ExceptionUtil.uncheck(() -> Thread.sleep((long) (Math.random() * 1000))))
        .doOnCompleted(() -> System.out.println("completed"))
        .doOnNext(e -> System.out.println(e))
        .subscribe();
  }
}
