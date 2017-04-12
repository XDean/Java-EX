package xdean.jex.extra.rx.op;

import java.util.function.Function;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import rx.Observable;
import rx.Observable.Operator;
import rx.Subscriber;

/**
 * Lift with this operator seem to apply the function to the observable.
 * 
 * <pre>
 * {@code
 *   Function<Observable<T>, Observable<F>> func = ...;
 * Observable<T> ob = ...;
 * ob.lift(new FunctionOperator<>(func));
 * //is same as
 * func.apply(ob);
 * }
 * </pre>
 * 
 * 
 * @author XDean
 *
 * @param <F>
 * @param <T>
 */
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class FunctionOperator<F, T> implements Operator<F, T> {

  public static <F, T> FunctionOperator<F, T> of(Function<Observable<T>, Observable<F>> func) {
    return new FunctionOperator<>(func);
  }

  Function<Observable<T>, Observable<F>> func;

  @Override
  public Subscriber<? super T> call(Subscriber<? super F> t) {
    Subscriber<T> sub = new InnerSubscriber(t);
    t.add(sub);
    return sub;
  }

  private class InnerSubscriber extends Subscriber<T> {
    Subscriber<? super T> sub;

    public InnerSubscriber(Subscriber<? super F> actual) {
      func.apply(Observable.unsafeCreate(s -> sub = s)).subscribe(actual);
    }

    @Override
    public void onNext(T t) {
      sub.onNext(t);
    }

    @Override
    public void onCompleted() {
      sub.onCompleted();
    }

    @Override
    public void onError(Throwable e) {
      sub.onError(e);
    }
  }
}
