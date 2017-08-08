package xdean.jex.util.collection;

import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import xdean.jex.extra.function.BiConsumerThrow;

public class MapUtil {
  public static <K, V> HashMap<K, V> newHashMap() {
    return new HashMap<>();
  }

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

  public static <K, V, T extends Throwable> void forEach(Map<K, V> map, BiConsumerThrow<K, V, T> action) throws T {
    Objects.requireNonNull(action);
    for (Map.Entry<K, V> entry : map.entrySet()) {
      K k;
      V v;
      try {
        k = entry.getKey();
        v = entry.getValue();
      } catch (IllegalStateException ise) {
        throw new ConcurrentModificationException(ise);
      }
      action.accept(k, v);
    }
  }

  /**
   * Get the value of the key. If absent, put the default value.
   *
   * @param map
   * @param key
   * @param defaultGetter
   * @return
   */
  public static <K, V> V getOrPutDefault(Map<K, V> map, K key, Supplier<V> defaultGetter) {
    V result = map.get(key);
    if (result == null) {
      V defaultValue = defaultGetter.get();
      map.put(key, defaultValue);
      return defaultValue;
    } else {
      return result;
    }
  }
}
