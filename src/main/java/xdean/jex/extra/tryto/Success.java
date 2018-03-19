package xdean.jex.extra.tryto;

import java.util.NoSuchElementException;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author liuwenzhe2008@qq.com
 *
 */
public class Success<T> extends Try<T> {

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
  public Try<T> foreach(Consumer<T> f) {
    f.accept(value);
    return this;
  }

  @Override
  public <U> Try<U> flatMap(Function<T, Try<U>> f) {
    try {
      return f.apply(value);
    } catch (Exception e) {
      return new Failure<>(e);
    }
  }

  @Override
  public <U> Try<U> map(Function<T, U> f) {
    return Try.to(() -> f.apply(value));
  }

  @Override
  public Try<T> filter(Predicate<T> p) {
    try {
      if (p.test(value)) {
        return this;
      } else {
        return new Failure<>(new NoSuchElementException("Predicate does not hold for " + value));
      }
    } catch (Exception e) {
      return new Failure<>(e);
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
    try {
      return s.apply(this.get());
    } catch (RuntimeException e) {
      return new Failure<>(e);
    }
  }

  @Override
  public Try<T> onException(Consumer<Exception> f) {
    // do nothing
    return this;
  }

}
