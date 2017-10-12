package xdean.jex.extra.rx2.nullable.source;

import io.reactivex.Flowable;
import io.reactivex.Observable;

/**
 * To Observable or Flowable
 *
 * @author XDean
 *
 * @param <T>
 */
public interface ObservableFlowable<T> {
  Observable<T> observable();

  Flowable<T> flowable();
}