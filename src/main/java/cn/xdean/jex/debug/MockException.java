package cn.xdean.jex.debug;

import java.util.function.Supplier;

import static cn.xdean.jex.lang.CacheUtil.cache;
import static cn.xdean.jex.lang.CacheUtil.set;
import static cn.xdean.jex.lang.function.FunctionAdapter.supplier;

/**
 * This class is used when developing. You can mock there happens an exception.
 *
 * @author XDean
 *
 */
public class MockException {
  public static <E extends Exception> void period(Object key, int period, E exception) throws E {
    period(key, period, supplier(exception));
  }

  /**
   * Mock exception happens by period
   *
   * @param key the unique key to decide these invocation are grouped
   * @param period how many invocations will occur an exception
   * @param exceptionFactory exception to throw
   * @throws E exception type
   */
  public static <E extends Exception> void period(Object key, int period, Supplier<E> exceptionFactory) throws E {
    int index = cache(MockException.class, key, () -> 0) + 1;
    set(MockException.class, key, index);
    if (index % period == 0) {
      throw exceptionFactory.get();
    }
  }

  public static <E extends Exception> void possible(double possible, E exception) throws E {
    possible(possible, supplier(exception));
  }

  /**
   * Mock exception happens by possibility
   *
   * @param possible
   * @param exceptionFactory
   * @throws E
   */
  public static <E extends Exception> void possible(double possible, Supplier<E> exceptionFactory) throws E {
    if (Math.random() < possible) {
      throw exceptionFactory.get();
    }
  }
}
