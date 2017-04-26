package xdean.jex.extra;

import lombok.EqualsAndHashCode;
import lombok.ToString;

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
