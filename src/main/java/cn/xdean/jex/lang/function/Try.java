package cn.xdean.jex.lang.function;

import cn.xdean.jex.lang.function.type.ActionE0;
import cn.xdean.jex.lang.function.type.FuncE0;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Try pattern, similar to Optional (learn from Scala)
 */
public interface Try<T> {

  /**
   * Constructs a `Try` using a code as a supplier.
   */
  static <T> Try<T> to(FuncE0<T, Exception> code) {
    try {
      return new Success<>(code.call());
    } catch (Exception e) {
      return new Failure<>(e);
    }
  }

  static Try<Boolean> to(ActionE0<Exception> code) {
    return Try.to(() -> {
      code.call();
      return true;
    });
  }

  static <T> Try<T> of(T value) {
    return new Success<>(value);
  }

  static <T extends Exception> Try<T> ofFailure(T value) {
    return new Failure<>(value);
  }

  /**********************************Methods*************************************/

  boolean isFailure();

  boolean isSuccess();

  T getOrElse(Supplier<T> defaultValue);

  T getOrElse(T defaultValue);

  Try<T> orElse(Supplier<Try<T>> defaultValue);

  Try<T> orElse(Try<T> defaultValue);

  T get();

  Try<T> doOnSuccess(Consumer<T> f);

  Try<T> doOnException(Consumer<Exception> f);

  <U> Try<U> flatMap(Function<T, Try<U>> f);

  <U> Try<U> map(Function<T, U> f);

  Try<T> filter(Predicate<T> p);

  Try<T> recoverWith(Function<Exception, Try<T>> f);

  Try<T> recover(Function<Exception, T> f);

  Optional<T> toOptional();

  Try<Exception> failed();

  <U> Try<U> transform(Function<T, Try<U>> s, Function<Exception, Try<U>> f);

  class Success<T> implements Try<T> {

    private final T value;

    Success(T value) {
      this.value = value;
    }

    @Override
    public boolean isFailure() {
      return false;
    }

    @Override
    public boolean isSuccess() {
      return true;
    }

    @Override
    public T get() {
      return value;
    }

    @Override
    public T getOrElse(T defaultValue) {
      return value;
    }

    @Override
    public T getOrElse(Supplier<T> defaultValue) {
      return value;
    }

    @Override
    public Try<T> orElse(Try<T> defaultValue) {
      return this;
    }

    @Override
    public Try<T> orElse(Supplier<Try<T>> defaultValue) {
      return this;
    }

    @Override
    public Try<T> doOnSuccess(Consumer<T> f) {
      f.accept(value);
      return this;
    }

    @Override
    public <U> Try<U> flatMap(Function<T, Try<U>> f) {
      return f.apply(value);
    }

    @Override
    public <U> Try<U> map(Function<T, U> f) {
      return to(() -> f.apply(value));
    }

    @Override
    public Try<T> filter(Predicate<T> p) {
      if (p.test(value)) {
        return this;
      } else {
        return new Failure<>(new NoSuchElementException("Predicate fail: " + value));
      }
    }

    @Override
    public Try<T> recoverWith(Function<Exception, Try<T>> f) {
      return this;
    }

    @Override
    public Try<T> recover(Function<Exception, T> f) {
      return this;
    }

    @Override
    public Try<Exception> failed() {
      return new Failure<>(new UnsupportedOperationException("Success.failed"));
    }

    @Override
    public <U> Try<U> transform(Function<T, Try<U>> s, Function<Exception, Try<U>> f) {
      return s.apply(this.get());
    }

    @Override
    public Try<T> doOnException(Consumer<Exception> f) {
      // do nothing
      return this;
    }

    @Override
    public Optional<T> toOptional() {
      return Optional.of(value);
    }
  }

  class Failure<T> implements Try<T> {

    private final Exception exception;

    Failure(Exception e) {
      exception = e;
    }

    @Override
    public boolean isFailure() {
      return true;
    }

    @Override
    public boolean isSuccess() {
      return false;
    }

    @Override
    public T get() {
      throw new IllegalStateException("The Try was failed", exception);
    }

    @Override
    public T getOrElse(T defaultValue) {
      return defaultValue;
    }

    @Override
    public T getOrElse(Supplier<T> defaultValue) {
      return defaultValue.get();
    }

    @Override
    public Try<T> orElse(Supplier<Try<T>> defaultValue) {
      return defaultValue.get();
    }

    @Override
    public Try<T> orElse(Try<T> defaultValue) {
      return defaultValue;
    }

    @Override
    public Try<T> doOnSuccess(Consumer<T> f) {
      // do nothing
      return this;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <U> Try<U> flatMap(Function<T, Try<U>> f) {
      return (Try<U>) this;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <U> Try<U> map(Function<T, U> f) {
      return (Try<U>) this;
    }

    @Override
    public Try<T> filter(Predicate<T> p) {
      return this;
    }

    @Override
    public Try<T> recoverWith(Function<Exception, Try<T>> f) {
      return f.apply(exception);
    }

    @Override
    public Try<T> recover(Function<Exception, T> f) {
      return Try.to(() -> f.apply(exception));
    }

    @Override
    public Try<Exception> failed() {
      return new Success<>(exception);
    }

    @Override
    public <U> Try<U> transform(Function<T, Try<U>> s, Function<Exception, Try<U>> f) {
      return f.apply(exception);
    }

    @Override
    public Try<T> doOnException(Consumer<Exception> f) {
      f.accept(exception);
      return this;
    }

    @Override
    public Optional<T> toOptional() {
      return Optional.empty();
    }
  }
}
