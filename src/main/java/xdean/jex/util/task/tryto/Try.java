package xdean.jex.util.task.tryto;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import xdean.jex.extra.function.ActionE0;
import xdean.jex.extra.function.FuncE0;
import xdean.jex.log.LogFactory;

/**
 * Try pattern, similar to Optional (learn from Scala)
 *
 * @author liuwenzhe2008@qq.com
 *
 */
public abstract class Try<T>{

  /**
   * Constructs a `Try` using a code as a supplier.
   */
  public static <T> Try<T> to(FuncE0<T, Exception> code, ActionE0<Exception> onFinally) {
    try {
      return new Success<>(code.call());
    } catch (Exception e) {
      return new Failure<>(e);
    } finally {
      if (onFinally != null) {
        try {
          onFinally.call();
        } catch (Exception e) {
          LogFactory.from(Try.class).trace().log(e.getMessage(), e);
        }
      }
    }
  }

  public static <T> Try<T> to(FuncE0<T, Exception> code) {
    return Try.to(code, null);
  }

  public static Try<Void> to(ActionE0<Exception> code, ActionE0<Exception> onFinally) {
    return Try.to(() -> {
      code.call();
      return null;
    }, onFinally);
  }

  public static Try<Void> to(ActionE0<Exception> code) {
    return Try.to(code, null);
  }

  public static <T> Try<T> of(T value) {
    return new Success<>(value);
  }

  public static <T extends Exception> Try<T> ofFailure(T value) {
    return new Failure<>(value);
  }

  /**
   * Returns `true` if the `Try` is a `Failure`, `false` otherwise.
   */
  public abstract boolean isFailure();

  /**
   * Returns `true` if the `Try` is a `Success`, `false` otherwise.
   */
  public abstract boolean isSuccess();

  /**
   * Returns the value from this `Success` or the given `default` argument if this is a `Failure`.
   * <p>
   * ''Note:'': This will throw an exception if it is not a success and default throws an exception.
   */
  public T getOrElse(Supplier<T> defaultValue) {
    return isSuccess() ? get() : defaultValue.get();
  }

  /**
   * Returns the value from this `Success` or the given `default` argument if this is a `Failure`.
   * <p>
   * ''Note:'': This will throw an exception if it is not a success and default throws an exception.
   */
  public T getOrElse(T defaultValue) {
    return isSuccess() ? get() : defaultValue;
  }

  /**
   * Returns this `Try` if it's a `Success` or the given `default` argument if this is a `Failure`.
   */
  public Try<T> orElse(Supplier<Try<T>> defaultValue) {
    try {
      return isSuccess() ? this : defaultValue.get();
    } catch (RuntimeException e) {
      return new Failure<>(e);
    }
  }

  /**
   * Returns this `Try` if it's a `Success` or the given `default` argument if this is a `Failure`.
   */
  public Try<T> orElse(Try<T> defaultValue) {
    return isSuccess() ? this : defaultValue;
  }

  /**
   * Returns the value from this `Success` or throws the exception if this is a `Failure`.
   */
  public abstract T get();

  /**
   * Applies the given function `f` if this is a `Success`, otherwise returns `Unit` if this is a `Failure`.
   * <p>
   * ''Note:'' If `f` throws, then this method may throw an exception.
   */
  public abstract Try<T> foreach(Consumer<T> f);

  public abstract Try<T> onException(Consumer<Exception> f);

  /**
   * Returns the given function applied to the value from this `Success` or returns this if this is a `Failure`.
   */
  public abstract <U> Try<U> flatMap(Function<T, Try<U>> f);

  /**
   * Maps the given function to the value from this `Success` or returns this if this is a `Failure`.
   */
  public abstract <U> Try<U> map(Function<T, U> f);

  /**
   * Converts this to a `Failure` if the predicate is not satisfied.
   */
  public abstract Try<T> filter(Predicate<T> p);

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`. This is like
   * `flatMap` for the exception.
   */
  public abstract Try<T> recoverWith(Function<Exception, Try<T>> f);

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`. This is like
   * map for the exception.
   */
  public abstract Try<T> recover(Function<Exception, T> f);

  /**
   * Returns `None` if this is a `Failure` or a `Some` containing the value if this is a `Success`.
   */
  public Optional<T> toOptional() {
    return isSuccess() ? Optional.ofNullable(get()) : Optional.empty();
  }

  /**
   * Inverts this `Try`. If this is a `Failure`, returns its exception wrapped in a `Success`. If this is a `Success`,
   * returns a `Failure` containing an `UnsupportedOperationException`.
   */
  public abstract Try<Exception> failed();

  /**
   * Completes this `Try` by applying the function `f` to this if this is of type `Failure`, or conversely, by applying
   * `s` if this is a `Success`.
   */
  public abstract <U> Try<U> transform(Function<T, Try<U>> s, Function<Exception, Try<U>> f);
}
