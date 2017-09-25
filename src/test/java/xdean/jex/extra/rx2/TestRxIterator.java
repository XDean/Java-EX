package xdean.jex.extra.rx2;

import static org.junit.Assert.assertEquals;
import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Iterator;

import org.junit.Test;

public class TestRxIterator {
  @Test
  public void testFlowable() throws Exception {
    Iterator<Integer> iterator = Flowable.range(0, 10)
        .to(RxIterator.flowableIterator());
    for (int i = 0; i < 10; i++) {
      assertEquals(i, iterator.next().intValue());
    }
  }

  @Test
  public void testObservable() throws Exception {
    Iterator<Integer> iterator = Observable.range(0, 10)
        .to(RxIterator.observableIterator());
    for (int i = 0; i < 10; i++) {
      assertEquals(i, iterator.next().intValue());
    }
  }
}
