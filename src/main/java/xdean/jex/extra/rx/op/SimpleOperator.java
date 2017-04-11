package xdean.jex.extra.rx.op;

import java.util.function.Function;

import lombok.AllArgsConstructor;
import rx.Observable.Operator;
import rx.Subscriber;

@AllArgsConstructor
public class SimpleOperator<F, T> implements Operator<F, T> {

  Function<Subscriber<? super F>, Subscriber<? super T>> subscriberFactory;

  @Override
  public Subscriber<? super T> call(Subscriber<? super F> t) {
    Subscriber<? super T> subscriber = subscriberFactory.apply(t);
    t.add(subscriber);
    return subscriber;
  }

}
