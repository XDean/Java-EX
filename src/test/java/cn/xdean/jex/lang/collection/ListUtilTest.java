package cn.xdean.jex.lang.collection;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class ListUtilTest {
  List<Integer> list = new ArrayList<>(Arrays.asList(0, 1, 2, 3, 4, 5));

  @Test
  public void testForEach() throws Exception {
    ListUtil.forEach(list, (a, b) -> assertEquals(a, b));
  }

  @Test
  public void testLastGet() throws Exception {
    for (int i = 0; i < list.size(); i++) {
      assertEquals(5 - i, ListUtil.lastGet(list, i).intValue());
    }
  }

  @Test
  public void testIfAbsent() throws Exception {
    assertFalse(ListUtil.addIfAbsent(list, 3));
    assertTrue(ListUtil.addIfAbsent(list, 10));
    assertFalse(ListUtil.addIfAbsent(list, 10));
  }
}
