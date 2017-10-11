package xdean.jex.extra.rx2.nullable.impl;

import io.reactivex.Flowable;
import io.reactivex.Observable;

import java.util.Arrays;

import xdean.jex.extra.rx2.nullable.NullPolicy;
import xdean.jex.extra.rx2.nullable.NullableHandler;

public class NullableArray<F> implements NullableHandler<F> {
  F[] originArray;

  public NullableArray(F[] originArray) {
    this.originArray = originArray;
  }

  @Override
  public <T> Converter<T> policy(NullPolicy<F, T> policy) {
    Converter<T> ob = new Converter<>();
    ob.policy(policy);
    return ob;
  }

  public class Converter<T> extends OFWithPolicy<F, T> {
    @Override
    public Observable<T> observable() {
      return Observable.fromIterable(policy.handle(Arrays.asList(originArray)));
    }

    @Override
    public Flowable<T> flowable() {
      return Flowable.fromIterable(policy.handle(Arrays.asList(originArray)));
    }
  }
}