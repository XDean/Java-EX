package xdean.jex.util.lang;

import static xdean.jex.util.function.FunctionAdapter.supplierToRunnable;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;
import xdean.jex.extra.Either;
import xdean.jex.extra.Wrapper;
import xdean.jex.extra.function.RunnableThrow;
import xdean.jex.extra.function.SupplierThrow;

@Slf4j
public class ExceptionUtil {
  public static <T extends Throwable, R> R throwIt(T t) throws T {
    throw t;
  }

  @SuppressWarnings("unchecked")
  public static <T extends Throwable> void throwAsUncheck(Throwable t) throws T {
    throw (T) t;
  }

  public static void uncheck(RunnableThrow<?> task) {
    try {
      task.run();
    } catch (Throwable t) {
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
    } catch (Throwable t) {
      log.trace("Dont catch", t);
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
    } catch (Throwable t) {
      try {
        return Optional.of((E) t);
      } catch (ClassCastException cce) {
        throw new RuntimeException("An unexcepted exception thrown.", t);
      }
    }
    return Optional.empty();
  }

  public static <T, E extends Exception> Either<T, E> throwToReturn(SupplierThrow<T, E> task) {
    Wrapper<T> w = new Wrapper<T>(null);
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
}
