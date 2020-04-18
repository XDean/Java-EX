package cn.xdean.jex.lang;

import org.junit.Test;

import java.util.Optional;

import static cn.xdean.jex.lang.collection.OptionalUtil.ifEmpty;
import static cn.xdean.jex.lang.collection.OptionalUtil.ifPresent;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

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
