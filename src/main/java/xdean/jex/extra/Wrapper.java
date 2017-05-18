package xdean.jex.extra;

import xdean.jex.extra.annotation.NotThreadSafe;
import lombok.EqualsAndHashCode;
import lombok.ToString;

@NotThreadSafe
@EqualsAndHashCode
@ToString
public class Wrapper<T> {
  private T value;

  public Wrapper(T value) {
    super();
    this.value = value;
  }

  public T get() {
    return value;
  }

  public void set(T t) {
    value = t;
  }

  public static <T> Wrapper<T> of(T t) {
    return new Wrapper<T>(t);
  }

  public static <T> Wrapper<T> empty() {
    return Wrapper.of(null);
  }
}
