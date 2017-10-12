package xdean.jex.extra.rx2.nullable.source;

import java.util.Optional;

import xdean.jex.extra.rx2.nullable.handler.NullHandler;
import xdean.jex.extra.rx2.nullable.handler.NullHandlers;

@SuppressWarnings("unchecked")
public interface NullableSource<F, D extends Generic<F>, O extends Generic<Optional<F>>> {
  <T> Generic<T> handler(NullHandler<F, T> handler);

  default D onNullDrop() {
    return (D) handler(NullHandlers.drop());
  }

  default O onNullWrap() {
    return (O) handler(NullHandlers.wrap());
  }

  default D onNullDefault(F defaultValue) {
    return (D) handler(NullHandlers.defaultValue(defaultValue));
  }

  default D onNullRun(Runnable action) {
    return (D) handler(NullHandlers.run(action));
  }
}