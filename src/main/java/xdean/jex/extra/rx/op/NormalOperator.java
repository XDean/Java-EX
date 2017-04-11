package xdean.jex.extra.rx.op;

import lombok.AllArgsConstructor;
import rx.Observable.Operator;
import rx.Subscriber;

public abstract class NormalOperator<R, T> implements Operator<R, T> {

  @Override
  public Subscriber<? super T> call(Subscriber<? super R> t) {
    NormalSubscriber ns = new NormalSubscriber(t);
    t.add(ns);
    return ns;
  }
  
  protected void onStart(Subscriber<? super R> actual){
    actual.onStart();
  }

  protected abstract void onNext(Subscriber<? super R> actual, T t);

  protected void onCompleted(Subscriber<? super R> actual) {
    actual.onCompleted();
  }

  protected void onError(Subscriber<? super R> actual, Throwable e) {
    actual.onError(e);
  }

  @AllArgsConstructor
  private class NormalSubscriber extends Subscriber<T> {
    Subscriber<? super R> actual;
    
    @Override
    public void onStart() {
      NormalOperator.this.onStart(actual);
    }

    @Override
    public void onNext(T t) {
      NormalOperator.this.onNext(actual, t);
    }

    @Override
    public void onCompleted() {
      NormalOperator.this.onCompleted(actual);
    }

    @Override
    public void onError(Throwable e) {
      NormalOperator.this.onError(actual, e);
    }
  }
}
