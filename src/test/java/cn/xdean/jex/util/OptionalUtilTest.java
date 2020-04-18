package cn.xdean.jex.util;

import static org.junit.Assert.*;
import static cn.xdean.jex.util.OptionalUtil.*;

import java.util.Optional;

import org.junit.Test;

public class OptionalUtilTest {
  @Test
  public void testIfEmpty() throws Exception {
    ifEmpty(Optional.of(1), () -> fail());
    int[] count = new int[1];
    ifEmpty(Optional.empty(), () -> count[0]++);
    assertEquals(1, count[0]);
  }

  @Test
  public void testIfPresent() throws Exception {
    ifPresent(Optional.empty(), i -> fail());
    int[] count = new int[1];
    ifPresent(Optional.of(10), i -> count[0] += i);
    assertEquals(10, count[0]);
  }
}
