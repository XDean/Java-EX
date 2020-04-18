package cn.xdean.jex.lang.collection;

import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.assertEquals;

public class MapUtilTest {
  @Test
  public void testNew() throws Exception {
    HashMap<Integer, Integer> map = MapUtil.newHashMap(new Integer[] { 1, 2 }, new Integer[] { 10, 20 });
    assertEquals(10, map.get(1).intValue());
    assertEquals(20, map.get(2).intValue());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testNewThrow() throws Exception {
    HashMap<Integer, Integer> map = MapUtil.newHashMap(new Integer[] { 1, 2, 3 }, new Integer[] {});
    assertEquals(10, map.get(1).intValue());
    assertEquals(20, map.get(2).intValue());
  }
}
