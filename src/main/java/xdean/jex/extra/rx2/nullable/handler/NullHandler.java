package xdean.jex.extra.rx2.nullable.handler;

import java.util.function.Function;

/**
 * Convert the nullable origin value. If apply return null again, the element will be ignored.
 *
 * @author XDean
 *
 * @param <F>
 * @param <T>
 */
public interface NullHandler<F, T> extends Function<F, T> {
  @Override
  T apply(F originValue);
}