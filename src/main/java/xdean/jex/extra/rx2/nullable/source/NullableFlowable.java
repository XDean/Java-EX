package xdean.jex.extra.rx2.nullable.source;

import java.util.Optional;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;

public interface NullableFlowable<F> extends NullableSource<F, GenericFlowable<F>, GenericFlowable<Optional<F>>> {
  @Override
  <T> GenericFlowable<T> handler(NullHandler<F, T> handler);
}