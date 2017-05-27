package xdean.jex.util.task;

import java.util.Optional;
import java.util.function.Supplier;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class If<T> {

  public static <T> If<T> that(boolean b) {
    return new If<>(b);
  }

  final boolean condition;
  T result;

  public If<T> todo(Runnable r) {
    if (condition) {
      r.run();
    }
    return this;
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

  public If<T> orbe(Supplier<T> s) {
    if (!condition) {
      result = s.get();
    }
    return this;
  }

  @Deprecated
  public boolean toBoolean() {
    return condition;
  }

  public boolean condition() {
    return condition;
  }

  public T result() {
    return result;
  }

  public Optional<T> safeResult() {
    return Optional.ofNullable(result);
  }
}