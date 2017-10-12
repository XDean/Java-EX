package xdean.jex.extra.rx2;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.junit.Test;

import xdean.jex.extra.rx2.nullable.NullPolicies;
import xdean.jex.extra.rx2.nullable.RxNullable;

public class TestRxNullable {
  @Test
  public void testDrop() throws Exception {
    RxNullable.fromArray(1, null, 2, null, 3)
        .observable(NullPolicies.drop())
        .test()
        .assertValues(1, 2, 3);
    RxNullable.fromArray(1, null, 2, null, 3)
        .flowable(NullPolicies.drop())
        .test()
        .assertValues(1, 2, 3);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testWrap() throws Exception {
    RxNullable.fromArray(1, null, 2, null, 3)
        .onNullWrap()
        .observable()
        .test()
        .assertValues(Optional.of(1), Optional.empty(), Optional.of(2), Optional.empty(), Optional.of(3));
    RxNullable.fromArray(1, null, 2, null, 3)
        .onNullWrap()
        .flowable()
        .test()
        .assertValues(Optional.of(1), Optional.empty(), Optional.of(2), Optional.empty(), Optional.of(3));
  }

  @Test
  public void testDefault() throws Exception {
    RxNullable.fromArray(1, null, 2, null, 3)
        .onNullDefault(10)
        .observable()
        .test()
        .assertValues(1, 10, 2, 10, 3);
    RxNullable.fromArray(1, null, 2, null, 3)
        .onNullDefault(10)
        .flowable()
        .test()
        .assertValues(1, 10, 2, 10, 3);
  }

  @Test
  public void testIteratable() throws Exception {
    RxNullable.fromIterable(Arrays.asList(1, null, 2, null, 3))
        .onNullDrop()
        .observable()
        .test()
        .assertValues(1, 2, 3);
    RxNullable.fromIterable(Arrays.asList(1, null, 2, null, 3))
        .onNullDrop()
        .flowable()
        .test()
        .assertValues(1, 2, 3);
  }

  @Test
  public void testCallable() throws Exception {
    RxNullable.fromCallable(() -> null)
        .onNullDrop()
        .observable()
        .test(true)
        .assertValueCount(0);
    RxNullable.fromCallable(() -> null)
        .onNullDrop()
        .flowable()
        .test()
        .assertValueCount(0);
  }

  @Test
  public void testFuture() throws Exception {
    FutureTask<Object> future = new FutureTask<>(() -> null);
    future.run();
    RxNullable.fromFuture(future)
        .onNullDrop()
        .observable()
        .test()
        .assertValueCount(0);
    RxNullable.fromFuture(future)
        .onNullDrop()
        .flowable()
        .test()
        .assertValueCount(0);
  }

  @Test
  public void testTimeout() throws Exception {
    FutureTask<Object> future = new FutureTask<>(() -> 1);
    RxNullable.fromFuture(future, 5, TimeUnit.MILLISECONDS)
        .onNullDrop()
        .observable()
        .test()
        .assertValueCount(0)
        .assertError(TimeoutException.class);
    RxNullable.fromFuture(future, 5, TimeUnit.MILLISECONDS)
        .onNullDrop()
        .flowable()
        .test()
        .assertValueCount(0)
        .assertError(TimeoutException.class);
  }

  @Test
  public void testPublisher() throws Exception {
    RxNullable.fromPublisher(s -> {
      s.onNext(1);
      s.onNext(null);
      s.onNext(2);
      s.onNext(null);
      s.onNext(3);
      s.onComplete();
    })
        .onNullDrop()
        .observable()
        .test()
        .assertValues(1, 2, 3);
    RxNullable.fromPublisher(s -> {
      s.onNext(1);
      s.onNext(null);
      s.onNext(2);
      s.onNext(null);
      s.onNext(3);
      s.onComplete();
    })
        .onNullDrop()
        .flowable()
        .test()
        .assertValues(1, 2, 3);
  }
}
