package xdean.jex.util.task;

import static xdean.jex.util.lang.ExceptionUtil.*;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import rx.Observable;
import rx.schedulers.Schedulers;
import xdean.jex.extra.Either;
import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.extra.function.SupplierThrow;

/**
 * Utility methods for task flow control.
 *
 * @author XDean
 *
 */
public class TaskUtil {

  public static void async(Runnable task) {
    Observable.just(task).observeOn(Schedulers.newThread()).subscribe(r -> r.run());
  }

  public static void todoAll(Runnable... tasks) {
    for (Runnable task : tasks) {
      task.run();
    }
  }

  /**
   * Return the first result of these tasks<br>
   * IGNORE EXCEPTIONS.
   *
   * @param tasks
   * @return can be null
   * @throws IllegalStateException If all tasks failed.
   */
  @SafeVarargs
  public static <T> T firstSuccess(SupplierThrow<T, ?>... tasks) throws IllegalStateException {
    for (SupplierThrow<T, ?> task : tasks) {
      Either<T, ?> res = throwToReturn(task);
      if (res.isLeft()) {
        return res.getLeft();
      }
    }
    throw new IllegalStateException("All tasks failed");
  }

  /**
   * Return the first non-null result of the given tasks or empty if all of them return null.<br>
   * IGNORE EXCEPTIONS.
   *
   * @param tasks
   * @return can be null
   */
  @SafeVarargs
  public static <T> Optional<T> firstNonNull(SupplierThrow<T, ?>... tasks) {
    for (SupplierThrow<T, ?> task : tasks) {
      T res = uncatch(task);
      if (res != null) {
        return Optional.of(res);
      }
    }
    return Optional.empty();
  }

  /**
   * Run the given tasks until any exception happen
   *
   * @param tasks
   * @return the exception
   */
  @SuppressWarnings("unchecked")
  @SafeVarargs
  public static <T extends Exception> Optional<T> firstFail(RunnableThrow<T>... tasks) {
    for (RunnableThrow<T> task : tasks) {
      try {
        task.run();
      } catch (Throwable t) {
        try {
          return Optional.of((T) t);
        } catch (ClassCastException cce) {
          throw new RuntimeException("An unexcepted exception thrown.", t);
        }
      }
    }
    return Optional.empty();
  }

  public static void andFinal(Runnable task, Runnable then) {
    try {
      task.run();
    } finally {
      then.run();
    }
  }

  public static <T> T andFinal(Supplier<T> task, Consumer<T> then) {
    T t = null;
    try {
      return t = task.get();
    } finally {
      then.accept(t);
    }
  }
}
