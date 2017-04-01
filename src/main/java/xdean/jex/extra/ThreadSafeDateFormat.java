package xdean.jex.extra;

import java.text.DateFormat;
import java.util.function.Supplier;

import lombok.experimental.Delegate;

public class ThreadSafeDateFormat {
  ThreadLocal<DateFormat> format;

  public ThreadSafeDateFormat(Supplier<DateFormat> factory) {
    format = new ThreadLocal<DateFormat>() {
      @Override
      protected DateFormat initialValue() {
        return factory.get();
      }
    };
  }

  @Delegate
  private DateFormat get() {
    return format.get();
  }
}
