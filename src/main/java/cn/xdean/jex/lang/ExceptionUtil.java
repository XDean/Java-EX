package cn.xdean.jex.lang;

import cn.xdean.jex.lang.collection.Either;
import cn.xdean.jex.lang.collection.Wrapper;
import cn.xdean.jex.lang.function.type.ActionE0;
import cn.xdean.jex.lang.function.type.FuncE0;
import xdean.jex.log.LogFactory;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Optional;
import java.util.function.Function;

import static cn.xdean.jex.lang.function.FunctionAdapter.supplierToRunnable;

public class ExceptionUtil {
  public static <T extends Throwable, R> R throwIt(T t) throws T {
    throw t;
  }

  @SuppressWarnings("unchecked")
  public static <T extends Throwable, R> R throwAsUncheck(Throwable t) throws T {
    throw (T) t;
  }

  public static void uncheck(ActionE0<?> task) {
    try {
      task.call();
    } catch (Exception t) {
      throwAsUncheck(t);
    }
  }

  public static <T> T uncheck(FuncE0<T, ?> task) {
    return supplierToRunnable(task, r -> uncheck(r));
  }

  public static boolean uncatch(ActionE0<?> task) {
    try {
      task.call();
      return true;
    } catch (Exception t) {
      LogFactory.from(ExceptionUtil.class).trace("Dont catch", t);
      return false;
    }
  }

  /**
   * @param task
   * @return can be null
   */
  public static <T> T uncatch(FuncE0<T, ?> task) {
    return supplierToRunnable(task, r -> uncatch(r));
  }

  @SuppressWarnings("unchecked")
  public static <E extends Exception> Optional<E> throwToReturn(ActionE0<E> task) {
    try {
      task.call();
    } catch (Exception t) {
      try {
        return Optional.of((E) t);
      } catch (ClassCastException cce) {
        throw new RuntimeException("An unexcepted exception thrown.", t);
      }
    }
    return Optional.empty();
  }

  public static <T, E extends Exception> Either<T, E> throwToReturn(FuncE0<T, E> task) {
    Wrapper<T> w = new Wrapper<>(null);
    return Either.rightOrDefault(throwToReturn(() -> w.set(task.call())), w.get());
  }

  public static String getStackTraceString(Throwable tr) {
    if (tr == null) {
      return "";
    }
    Throwable t = tr;
    while (t.getCause() != null) {
      t = t.getCause();
    }
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    t.printStackTrace(pw);
    pw.flush();
    return sw.toString();
  }

  public static <E extends Exception> void wrapException(Function<Exception, E> wrapper, ActionE0<?> task)
      throws E {
    try {
      task.call();
    } catch (Exception e) {
      throw wrapper.apply(e);
    }
  }

  public static <E extends Exception, T> T wrapException(Function<Exception, E> wrapper, FuncE0<T, ?> task)
      throws E {
    try {
      return task.call();
    } catch (Exception e) {
      throw wrapper.apply(e);
    }
  }
}
