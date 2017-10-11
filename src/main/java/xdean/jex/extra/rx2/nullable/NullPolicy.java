package xdean.jex.extra.rx2.nullable;

import java.util.function.Function;

public interface NullPolicy<F, T> extends Function<F, T> {
  @Override
  T apply(F originValue);
}