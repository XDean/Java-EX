package cn.xdean.jex.lang.convert;

public interface StringConverter<T> {
  String toString(T object);

  T fromString(String string);
}
