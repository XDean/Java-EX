package xdean.jex.extra.rx2.nullable;

public interface NullableHandler<F> {
  <T> ObservableFlowable<T> policy(NullPolicy<F, T> policy);
}