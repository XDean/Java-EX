package xdean.jex.util.lang;

import static xdean.jex.util.function.FunctionAdapter.supplierToRunnable;
import static xdean.jex.util.log.LogUtil.trace;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Optional;
import java.util.function.Function;

import xdean.jex.extra.Either;
import xdean.jex.extra.Wrapper;
import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.extra.function.SupplierThrow;

public class ExceptionUtil {
  public static <T extends Throwable, R> R throwIt(T t) throws T {
    throw t;
  }

  @SuppressWarnings("unchecked")
  public static <T extends Throwable, R> R throwAsUncheck(Throwable t) throws T {
    throw (T) t;
  }

  public static void uncheck(RunnableThrow<?> task) {
    try {
      task.run();
    } catch (Exception t) {
      throwAsUncheck(t);
    }
  }

  public static <T> T uncheck(SupplierThrow<T, ?> task) {
    return supplierToRunnable(task, r -> uncheck(r));
  }

  public static boolean uncatch(RunnableThrow<?> task) {
    try {
      task.run();
      return true;
    } catch (Exception t) {
      trace().log("Dont catch", t);
      return false;
    }
  }

  /**
   * @param task
   * @return can be null
   */
  public static <T> T uncatch(SupplierThrow<T, ?> task) {
    return supplierToRunnable(task, r -> uncatch(r));
  }

  @SuppressWarnings("unchecked")
  public static <E extends Exception> Optional<E> throwToReturn(RunnableThrow<E> task) {
    try {
      task.run();
    } catch (Exception t) {
      try {
        return Optional.of((E) t);
      } catch (ClassCastException cce) {
        throw new RuntimeException("An unexcepted exception thrown.", t);
      }
    }
    return Optional.empty();
  }

  public static <T, E extends Exception> Either<T, E> throwToReturn(SupplierThrow<T, E> task) {
    Wrapper<T> w = new Wrapper<>(null);
    return Either.rightOrDefault(throwToReturn(() -> w.set(task.get())), w.get());
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

  public static <E extends Exception> void wrapException(Function<Exception, E> wrapper, RunnableThrow<?> task)
      throws E {
    try {
      task.run();
    } catch (Exception e) {
      throw wrapper.apply(e);
    }
  }
}
