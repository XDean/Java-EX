package xdean.jex.extra.rx;

import rx.Observable;
import rx.Subscription;

public class ObservableWrapper implements Subscription {
  private final Observable<?> ob;
  private Subscription subscription;

  public ObservableWrapper(Observable<?> ob) {
    this.ob = ob
        .onErrorResumeNext(e -> {
          unsubscribe();
          return Observable.error(e);
        })
        .doOnCompleted(() -> unsubscribe());
  }

  public void subscribe() {
    unsubscribe();
    subscription = ob.subscribe();
  }

  @Override
  public void unsubscribe() {
    Subscription subscription = this.subscription;
    this.subscription = null;
    if (subscription != null && !subscription.isUnsubscribed()) {
      subscription.unsubscribe();
    }
  }

  @Override
  public boolean isUnsubscribed() {
    return subscription == null;
  }
}
