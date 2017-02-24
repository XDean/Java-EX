package xdean.jex.util.collection;

import java.util.HashMap;

import lombok.experimental.UtilityClass;

@UtilityClass
public class MapUtil {
  public <K, V> HashMap<K, V> newHashMap() {
    return new HashMap<>();
  }

  public <K, V> HashMap<K, V> newHashMap(K[] keys, V[] values) {
    if (keys.length > values.length) {
      throw new IllegalArgumentException("Values is less than keys");
    }
    HashMap<K, V> map = new HashMap<>();
    for (int i = 0; i < keys.length; i++) {
      map.put(keys[i], values[i]);
    }
    return map;
  }
}
