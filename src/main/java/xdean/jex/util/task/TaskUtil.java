package xdean.jex.util.task;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import rx.Observable;
import rx.schedulers.Schedulers;
import xdean.jex.extra.Either;

@Slf4j
@UtilityClass
public class TaskUtil {

  public interface TaskWithThrowable<V, T extends Throwable> {
    V call() throws T;
  }

  public interface RunnableWithThrowable<T extends Throwable> {
    void run() throws T;
  }

  public interface TaskWithException<V> extends TaskWithThrowable<V, Exception> {
  }

  public interface RunnableWithException extends RunnableWithThrowable<Exception> {
  }

  public void async(Runnable task) {
    Observable.just(task).observeOn(Schedulers.newThread()).subscribe(r -> r.run());
  }

  public <T> T uncheck(TaskWithException<T> task) {
    try {
      return task.call();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public void uncheck(RunnableWithException task) {
    try {
      task.run();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public boolean uncatch(RunnableWithException task) {
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
  public <T> T uncatch(TaskWithException<T> task) {
    try {
      return task.call();
    } catch (Exception e) {
      log.trace("Dont catch", e);
    }
    return null;
  }

  // public Optional<Exception> throwToReturn(RunnableWithException task)
  // {
  // try {
  // task.run();
  // } catch (Exception e) {
  // return Optional.of(e);
  // }
  // return Optional.empty();
  // }

  // public <T> Either<T, Exception> throwToReturn(TaskWithException<T>
  // task) {
  // try {
  // T t = task.call();
  // return Either.left(t);
  // } catch (Exception e) {
  // return Either.right(e);
  // }
  // }

  @SuppressWarnings("unchecked")
  public <E extends Exception> Optional<E> throwToReturn(RunnableWithThrowable<E> task) {
    try {
      task.run();
    } catch (Exception e) {
      try {
        return Optional.of((E) e);
      } catch (ClassCastException cce) {
        throw new RuntimeException("An unexcepted exception thrown.", e);
      }
    }
    return Optional.empty();
  }

  @SuppressWarnings("unchecked")
  public <T, E extends Exception> Either<T, E> throwToReturn(TaskWithThrowable<T, E> task) {
    try {
      T t = task.call();
      return Either.left(t);
    } catch (Exception e) {
      try {
        return Either.right((E) e);
      } catch (ClassCastException cce) {
        throw new RuntimeException("An unexcepted exception thrown.", e);
      }
    }
  }

  public void todoAll(Runnable... tasks) {
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
  public <T> T firstSuccess(TaskWithException<T>... tasks) {
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
  public Optional<Exception> firstFail(RunnableWithException... tasks) {
    for (RunnableWithException task : tasks) {
      try {
        task.run();
      } catch (Exception e) {
        return Optional.of(e);
      }
    }
    return Optional.empty();
  }

  public void andFinal(Runnable task, Runnable then) {
    try {
      task.run();
    } finally {
      then.run();
    }
  }

  public <T> T andFinal(Supplier<T> task, Consumer<T> then) {
    T t = null;
    try {
      return t = task.get();
    } finally {
      then.accept(t);
    }
  }

  public boolean ifTodo(boolean b, Runnable todo) {
    if (b) {
      todo.run();
    }
    return b;
  }

  public boolean ifTodo(boolean b, Runnable todo, Runnable elseTodo) {
    if (b) {
      todo.run();
    } else {
      elseTodo.run();
    }
    return b;
  }
}
