package cn.xdean.jex.lang.function;

import cn.xdean.jex.lang.ExceptionUtil;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Supplier;

public final class If<T> {

  public static <T> If<T> that(boolean b) {
    return new If<>(b);
  }

  private static final Object NULL_RESULT = new Object();

  private final If<T> parent;
  private final boolean condition;
  @SuppressWarnings("unchecked")
  private T result = (T) NULL_RESULT;

  public If(boolean condition) {
    this.condition = condition;
    this.parent = null;
  }

  public If(boolean condition, If<T> parent) {
    this.condition = condition;
    this.parent = parent;
  }

  public If<T> or(Supplier<Boolean> b) {
    return new If<>(condition || b.get(), this);
  }

  public If<T> and(Supplier<Boolean> b) {
    return new If<>(condition && b.get(), this);
  }

  public If<T> or(boolean b) {
    return new If<>(condition || b, this);
  }

  public If<T> and(boolean b) {
    return new If<>(condition && b, this);
  }

  public If<T> todo(Runnable r) {
    if (condition) {
      r.run();
    }
    return this;
  }

  public If<T> tobe(T t) {
    return tobe(() -> t);
  }

  public If<T> tobe(Supplier<T> s) {
    if (condition) {
      result = s.get();
    }
    return this;
  }

  public If<T> ordo(Runnable r) {
    if (!condition) {
      r.run();
    }
    return this;
  }

  public If<T> orbe(T t) {
    return orbe(() -> t);
  }

  public If<T> orbe(Supplier<T> s) {
    if (!condition) {
      result = s.get();
    }
    return this;
  }

  public boolean condition() {
    return condition;
  }

  public If<T> end(boolean withResult) {
    if (parent != null) {
      if (withResult && result != NULL_RESULT) {
        parent.result = result;
      }
      return parent;
    } else {
      throw new IllegalStateException("End more than if!");
    }
  }

  public If<T> end() {
    return end(false);
  }

  /**
   * Get the result.
   *
   * @return
   * @throws NoSuchElementException If there is no result be set.
   */
  public T result() throws NoSuchElementException {
    if (result == NULL_RESULT) {
      if (parent != null) {
        return parent.result();
      }
      throw new NoSuchElementException("There is no result");
    }
    return result;
  }

  /**
   * Get the result without exception.<br>
   * Note that this method don't distinguish no result and null result.
   *
   * @return
   */
  public Optional<T> safeResult() {
    return Optional.ofNullable(ExceptionUtil.uncatch(() -> result()));
  }
}