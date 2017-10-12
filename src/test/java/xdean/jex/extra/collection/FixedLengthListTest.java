package xdean.jex.extra.collection;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

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
