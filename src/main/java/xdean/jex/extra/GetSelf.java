package xdean.jex.extra;

/**
 * Useful class for build chain and extendible API.
 *
 * @author XDean
 *
 * @param <T>
 */
public interface GetSelf<T> {
  @SuppressWarnings("unchecked")
  default T getSelf() {
    return (T) this;
  }
}
