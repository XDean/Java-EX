package xdean.jex.extra.rx2.nullable.impl;

import xdean.jex.extra.rx2.nullable.NullPolicy;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

abstract class OFWithPolicy<F, T> implements ObservableFlowable<T> {
  protected NullPolicy<F, T> policy;

  public void policy(NullPolicy<F, T> policy) {
    this.policy = policy;
  }
}