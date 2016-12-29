package xdean.jex.util.task;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import lombok.extern.slf4j.Slf4j;
import rx.Observable;
import rx.schedulers.Schedulers;
import xdean.jex.extra.Either;

@Slf4j
public enum TaskUtil {
  ;

  public static interface TaskWithException<T> {
    T call() throws Exception;
  }

  public static interface RunnableWithException {
    void run() throws Exception;
  }

  public static void async(Runnable task) {
    Observable.just(task).observeOn(Schedulers.newThread()).subscribe(r -> r.run());
  }

  public static <T> T uncheck(TaskWithException<T> task) {
    try {
      return task.call();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public static void uncheck(RunnableWithException task) {
    try {
      task.run();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public static boolean uncatch(RunnableWithException task) {
    try {
      task.run();
      return true;
    } catch (Exception e) {
      log.trace("Dont catch", e);
      return false;
    }
  }

  /**
   * 
   * @param task
   * @return can be null
   */
  public static <T> T uncatch(TaskWithException<T> task) {
    try {
      return task.call();
    } catch (Exception e) {
      log.trace("Dont catch", e);
    }
    return null;
  }

  public static Optional<Exception> throwToReturn(RunnableWithException task) {
    try {
      task.run();
    } catch (Exception e) {
      return Optional.of(e);
    }
    return Optional.empty();
  }

  public static <T> Either<T, Exception> throwToReturn(TaskWithException<T> task) {
    try {
      T t = task.call();
      return Either.left(t);
    } catch (Exception e) {
      return Either.right(e);
    }
  }

  public static void todoAll(Runnable... tasks) {
    for (Runnable task : tasks) {
      task.run();
    }
  }

  /**
   * Return the first NonNull result of these tasks
   * 
   * @param tasks
   * @return can be null
   */
  @SafeVarargs
  public static <T> T firstSuccess(TaskWithException<T>... tasks) {
    for (TaskWithException<T> task : tasks) {
      T result = uncatch(task);
      if (result != null) {
        return result;
      }
    }
    return null;
  }

  /**
   * Run the given tasks until any exception happen
   * 
   * @param tasks
   * @return the exception
   */
  public static Optional<Exception> firstFail(RunnableWithException... tasks) {
    for (RunnableWithException task : tasks) {
      try {
        task.run();
      } catch (Exception e) {
        return Optional.of(e);
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

  public static void ifTodo(boolean b, Runnable todo) {
    if (b) {
      todo.run();
    }
  }

  public static void ifTodo(boolean b, Runnable todo, Runnable elseTodo) {
    if (b) {
      todo.run();
    } else {
      elseTodo.run();
    }
  }
}
