package cn.xdean.jex.extra.collection;

import java.util.Objects;

import cn.xdean.jex.extra.annotation.marker.NotThreadSafe;

@NotThreadSafe
public class Wrapper<T> {

  public static <T> Wrapper<T> of(T t) {
    return new Wrapper<>(t);
  }

  public static <T> Wrapper<T> empty() {
    return Wrapper.of(null);
  }

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

  @Override
  public int hashCode() {
    return Objects.hash(value);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    } else if (obj == null) {
      return false;
    } else if (!(obj instanceof Wrapper)) {
      return false;
    }
    Wrapper<?> other = (Wrapper<?>) obj;
    return Objects.equals(value, other.value);
  }

  @Override
  public String toString() {
    return "Wrapper [value=" + value + "]";
  }
}
