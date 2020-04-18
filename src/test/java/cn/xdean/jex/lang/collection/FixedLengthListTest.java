package cn.xdean.jex.lang.collection;

import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class FixedLengthListTest {
  @Test
  public void testNotFull() {
    List<Integer> list = new FixedLengthList<>(5);
    list.add(1);
    assertEquals(1, list.size());
    assertArrayEquals(new Integer[] { 1 }, list.toArray());
  }

  @Test
  public void testFull() {
    List<Integer> list = new FixedLengthList<>(5);
    String answer = "";
    for (int i = 0; i < 10; i++) {
      list.add(i);
      answer += list.get(0);
    }
    assertEquals(5, list.size());
    assertEquals("0000012345", answer);
    assertArrayEquals(new Integer[] { 5, 6, 7, 8, 9 }, list.toArray());
  }
}
