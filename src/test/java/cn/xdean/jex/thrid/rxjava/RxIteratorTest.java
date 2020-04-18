package cn.xdean.jex.thrid.rxjava;

import io.reactivex.Flowable;
import io.reactivex.Observable;
import io.reactivex.schedulers.Schedulers;
import org.junit.Test;

import java.util.Iterator;

import static org.junit.Assert.assertEquals;

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
