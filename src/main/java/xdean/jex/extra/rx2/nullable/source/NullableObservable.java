package xdean.jex.extra.rx2.nullable.source;

import java.util.Optional;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;

public interface NullableObservable<F> extends NullableSource<F, GenericObservable<F>, GenericObservable<Optional<F>>> {
  @Override
  <T> GenericObservable<T> handler(NullHandler<F, T> handler);
}