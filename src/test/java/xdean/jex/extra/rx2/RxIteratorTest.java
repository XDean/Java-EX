package xdean.jex.extra.rx2;

import static org.junit.Assert.*;
import io.reactivex.Flowable;
import io.reactivex.Observable;
import io.reactivex.schedulers.Schedulers;

import java.util.Iterator;

import org.junit.Test;

public class RxIteratorTest {

  @Test
  public void testObservable() throws Exception {
    Iterator<Integer> iterator = Observable.range(0, 10)
        .to(RxIterator.observableIterator());
    int i = 0;
    while (iterator.hasNext()) {
      assertEquals(i++, iterator.next().intValue());
    }
  }

  @Test
  public void testFlowable() throws Exception {
    Iterator<Integer> iterator = Flowable.range(0, 10)
        .to(RxIterator.flowableIterator());
    int i = 0;
    while (iterator.hasNext()) {
      assertEquals(i++, iterator.next().intValue());
    }
  }

  @Test
  public void testObservableAsync() throws Exception {
    Iterator<Integer> iterator = Observable.range(0, 10)
        .to(RxIterator.observableIterator(Schedulers.io()));
    int i = 0;
    while (iterator.hasNext()) {
      assertEquals(i++, iterator.next().intValue());
    }
  }

  @Test(expected = RuntimeException.class)
  public void testObservableError() throws Exception {
    Iterator<Object> iterator = Observable.error(new Exception()).to(RxIterator.observableIterator());
    iterator.hasNext();
  }

  @Test(expected = RuntimeException.class)
  public void testFlowableError() throws Exception {
    Iterator<Object> iterator = Flowable.error(new Exception()).to(RxIterator.flowableIterator());
    iterator.hasNext();
  }
}
