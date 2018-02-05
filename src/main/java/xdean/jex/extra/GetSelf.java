package xdean.jex.extra;

/**
 * Useful interface to build chain and extendible API.
 *
 * @author XDean
 */
public interface GetSelf<T> {
  @SuppressWarnings("unchecked")
  default T getSelf() {
    return (T) this;
  }
}
