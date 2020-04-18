package cn.xdean.jex.lang.convert;

import org.junit.Test;

import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;

import static org.junit.Assert.assertEquals;

public class StringConvertersTest {

  @Test
  public void testDouble() throws Exception {
    StringConverter<Double> sc = StringConverters.forDouble();
    assertEquals(1.0, sc.fromString("1.0d"), 0.0);
    assertEquals("1.0", sc.toString(1.0d));
    assertEquals(0.0, sc.fromString("error"), 0.0);
  }

  @Test
  public void testInteger() throws Exception {
    StringConverter<Integer> sc = StringConverters.forInteger();
    assertEquals(1, sc.fromString("1").intValue());
    assertEquals("1", sc.toString(1));
    assertEquals(0, sc.fromString("error").intValue());
  }

  @Test
  public void testOptionalDouble() throws Exception {
    StringConverter<OptionalDouble> sc = StringConverters.optionalDouble();
    assertEquals(OptionalDouble.of(1.0d), sc.fromString("1.0d"));
    assertEquals("1.0", sc.toString(OptionalDouble.of(1.0d)));
    assertEquals(OptionalDouble.empty(), sc.fromString("error"));
  }

  @Test
  public void testOptionalInt() throws Exception {
    StringConverter<OptionalInt> sc = StringConverters.optionalInt();
    assertEquals(OptionalInt.of(1), sc.fromString("1"));
    assertEquals("1", sc.toString(OptionalInt.of(1)));
    assertEquals(OptionalInt.empty(), sc.fromString("error"));
  }

  @Test
  public void testOptional() throws Exception {
    StringConverter<Optional<String>> sc = StringConverters.optional(
            v -> v.startsWith("123") ? "abc" + v.substring(3) : null,
            s -> s.startsWith("abc") ? "123" + s.substring(3) : null
    );
    assertEquals(Optional.of("123d"), sc.fromString("abcd"));
    assertEquals("abc45", sc.toString(Optional.of("12345")));
    assertEquals(Optional.empty(), sc.fromString("cbad"));
    assertEquals("", sc.toString(Optional.of("3214")));
  }
}
