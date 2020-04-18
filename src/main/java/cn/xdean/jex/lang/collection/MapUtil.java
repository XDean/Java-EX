package cn.xdean.jex.lang.collection;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class MapUtil {
  public static <K, V> HashMap<K, V> newHashMap(K[] keys, V[] values) {
    if (keys.length > values.length) {
      throw new IllegalArgumentException("Values is less than keys");
    }
    HashMap<K, V> map = new HashMap<>();
    for (int i = 0; i < keys.length; i++) {
      map.put(keys[i], values[i]);
    }
    return map;
  }

  /**
   * Get the value of the key. If absent, put the default value.
   *
   * @param map
   * @param key
   * @param defaultGetter
   * @return
   */
  @Deprecated
  public static <K, V> V getOrPutDefault(Map<K, V> map, K key, Supplier<V> defaultGetter) {
    return map.computeIfAbsent(key, k -> defaultGetter.get());
  }
}
