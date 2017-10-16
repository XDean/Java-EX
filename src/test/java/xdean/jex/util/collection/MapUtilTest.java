package xdean.jex.util.collection;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

public class MapUtilTest {
  @Test
  public void testGetOrPutDefault() throws Exception {
    Map<Integer, Integer> map = new HashMap<>();
    map.put(1, 1);
    map.put(2, 2);
    map.put(3, 3);
    assertEquals(3, MapUtil.getOrPutDefault(map, 3, () -> 10).intValue());
    assertEquals(10, MapUtil.getOrPutDefault(map, 5, () -> 10).intValue());
    assertTrue(map.containsKey(5));
  }

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
