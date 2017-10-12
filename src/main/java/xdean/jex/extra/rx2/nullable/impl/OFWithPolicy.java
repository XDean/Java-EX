package xdean.jex.extra.rx2.nullable.impl;

import xdean.jex.extra.rx2.nullable.NullHandler;
import xdean.jex.extra.rx2.nullable.ObservableFlowable;

abstract class OFWithPolicy<F, T> implements ObservableFlowable<T> {
  protected NullHandler<F, T> handler;

  public ObservableFlowable<T> handler(NullHandler<F, T> handler) {
    this.handler = handler;
    return this;
  }
}