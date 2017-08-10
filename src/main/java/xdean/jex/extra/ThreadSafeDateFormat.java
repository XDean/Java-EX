package xdean.jex.extra;

import java.text.AttributedCharacterIterator;
import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.function.Supplier;

public class ThreadSafeDateFormat extends DateFormat{
  ThreadLocal<DateFormat> format;

  public ThreadSafeDateFormat(Supplier<DateFormat> factory) {
    format = new ThreadLocal<DateFormat>() {
      @Override
      protected DateFormat initialValue() {
        return factory.get();
      }
    };
  }

  private DateFormat get() {
    return format.get();
  }

  @Override
  public AttributedCharacterIterator formatToCharacterIterator(Object obj) {
    return get().formatToCharacterIterator(obj);
  }

  @Override
  public Object parseObject(String source) throws ParseException {
    return get().parseObject(source);
  }

  @Override
  public StringBuffer format(Date date, StringBuffer toAppendTo, FieldPosition fieldPosition) {
    return get().format(date, toAppendTo, fieldPosition);
  }

  @Override
  public Date parse(String source) throws ParseException {
    return get().parse(source);
  }

  @Override
  public Date parse(String source, ParsePosition pos) {
    return get().parse(source, pos);
  }

  @Override
  public Object parseObject(String source, ParsePosition pos) {
    return get().parseObject(source, pos);
  }

  @Override
  public void setCalendar(Calendar newCalendar) {
    get().setCalendar(newCalendar);
  }

  @Override
  public Calendar getCalendar() {
    return get().getCalendar();
  }

  @Override
  public void setNumberFormat(NumberFormat newNumberFormat) {
    get().setNumberFormat(newNumberFormat);
  }

  @Override
  public NumberFormat getNumberFormat() {
    return get().getNumberFormat();
  }

  @Override
  public void setTimeZone(TimeZone zone) {
    get().setTimeZone(zone);
  }

  @Override
  public TimeZone getTimeZone() {
    return get().getTimeZone();
  }

  @Override
  public void setLenient(boolean lenient) {
    get().setLenient(lenient);
  }

  @Override
  public boolean isLenient() {
    return get().isLenient();
  }
}
