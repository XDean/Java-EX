package xdean.jex.util.task;

import static xdean.jex.util.lang.ExceptionUtil.throwToReturn;
import static xdean.jex.util.lang.ExceptionUtil.uncatch;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import io.reactivex.Observable;
import io.reactivex.schedulers.Schedulers;
import xdean.jex.extra.Either;
import xdean.jex.extra.function.ActionE0;
import xdean.jex.extra.function.FuncE0;

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
  public static <T> T firstSuccess(FuncE0<T, ?>... tasks) throws IllegalStateException {
    for (FuncE0<T, ?> task : tasks) {
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
  public static <T> Optional<T> firstNonNull(FuncE0<T, ?>... tasks) {
    for (FuncE0<T, ?> task : tasks) {
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
  @SafeVarargs
  @SuppressWarnings("unchecked")
  public static <T extends Exception> Optional<T> firstFail(ActionE0<T>... tasks) {
    for (ActionE0<T> task : tasks) {
      try {
        task.call();
      } catch (Exception t) {
        return Optional.of((T) t);
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
