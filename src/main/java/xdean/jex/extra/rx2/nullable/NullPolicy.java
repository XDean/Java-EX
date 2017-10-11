package xdean.jex.extra.rx2.nullable;

public interface NullPolicy<F, T> {
  Iterable<T> handle(Iterable<F> iterator);
}