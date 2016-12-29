package xdean.jex.extra;

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

  @Override
  public String toString() {
    return value.toString();
  }

  @Override
  public int hashCode() {
    return value.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    Wrapper<?> other = (Wrapper<?>) obj;
    if (value == null) {
      if (other.value != null) {
        return false;
      }
    } else if (!value.equals(other.value)) {
      return false;
    }
    return true;
  }

  public static <T> Wrapper<T> of(T t) {
    return new Wrapper<T>(t);
  }
}
