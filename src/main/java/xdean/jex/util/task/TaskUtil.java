package xdean.jex.util.task;

import static xdean.jex.util.function.FunctionAdapter.supplierToRunnable;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import lombok.AllArgsConstructor;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import rx.Observable;
import rx.schedulers.Schedulers;
import xdean.jex.extra.Either;
import xdean.jex.extra.Wrapper;
import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.extra.function.SupplierThrow;

@Slf4j
@UtilityClass
public class TaskUtil {

  public void async(Runnable task) {
    Observable.just(task).observeOn(Schedulers.newThread()).subscribe(r -> r.run());
  }

  public void uncheck(RunnableThrow<?> task) {
    try {
      task.run();
    } catch (Throwable t) {
      throw new RuntimeException(t);
    }
  }

  public <T> T uncheck(SupplierThrow<T, ?> task) {
    return supplierToRunnable(task, r -> uncheck(r));
  }

  public boolean uncatch(RunnableThrow<?> task) {
    try {
      task.run();
      return true;
    } catch (Throwable t) {
      log.trace("Dont catch", t);
      return false;
    }
  }

  /**
   * 
   * @param task
   * @return can be null
   */
  public <T> T uncatch(SupplierThrow<T, ?> task) {
    return supplierToRunnable(task, r -> uncatch(r));
  }

  @SuppressWarnings("unchecked")
  public <E extends Throwable> Optional<E> throwToReturn(RunnableThrow<E> task) {
    try {
      task.run();
    } catch (Throwable t) {
      try {
        return Optional.of((E) t);
      } catch (ClassCastException cce) {
        throw new RuntimeException("An unexcepted exception thrown.", t);
      }
    }
    return Optional.empty();
  }

  public <T, E extends Exception> Either<T, E> throwToReturn(SupplierThrow<T, E> task) {
    Wrapper<T> w = new Wrapper<T>(null);
    return Either.rightOrDefault(throwToReturn(() -> w.set(task.get())), w.get());
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
  public <T> T firstSuccess(SupplierThrow<T, ?>... tasks) {
    for (SupplierThrow<T, ?> task : tasks) {
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
  @SuppressWarnings("unchecked")
  @SafeVarargs
  public <T extends Throwable> Optional<T> firstFail(RunnableThrow<T>... tasks) {
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

  @AllArgsConstructor
  public class IfWrapper {
    boolean condition;

    public IfWrapper todo(Runnable r) {
      if (condition) {
        r.run();
      }
      return this;
    }

    public IfWrapper otherwise(Runnable r) {
      if (!condition) {
        r.run();
      }
      return this;
    }

    public boolean toBoolean() {
      return condition;
    }
  }

  public IfWrapper ifThat(boolean b) {
    return new IfWrapper(b);
  }

  @Deprecated
  public boolean ifTodo(boolean b, Runnable todo) {
    return ifThat(b).todo(todo).toBoolean();
  }

  @Deprecated
  public boolean ifTodo(boolean b, Runnable todo, Runnable elseTodo) {
    return ifThat(b).todo(todo).otherwise(elseTodo).toBoolean();
  }
}
