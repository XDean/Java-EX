package xdean.jex.util.cache;

import java.util.Map;
import java.util.Optional;
import java.util.WeakHashMap;
import java.util.function.Supplier;

@SuppressWarnings("unchecked")
public class CacheUtil {

  private static final Map<Object, Map<Object, Object>> CACHE_MAP = new WeakHashMap<>();

  public static <K, V> V cache(K key, Supplier<V> factory) {
    return cache(key.getClass(), key, factory);
  }

  public static <K, V> V cache(Object owner, K key, Supplier<V> factory) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return (V) map.get(key);
    }
    V v = factory.get();
    map.put(key, v);
    return v;
  }

  public static <K, V> Optional<V> get(K key) {
    return get(key.getClass(), key);
  }

  public static <K, V> Optional<V> get(Object owner, K key) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return Optional.of((V) map.get(key));
    }
    return Optional.empty();
  }

  public static <K, V> void set(K key, V value) {
    set(key.getClass(), key, value);
  }

  public static <K, V> void set(Object owner, K key, V value) {
    Map<Object, Object> map = getMap(owner);
    map.put(key, value);
  }

  public static <K, V> Optional<V> remove(K key) {
    return remove(key.getClass(), key);
  }

  public static <K, V> Optional<V> remove(Object owner, K key) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return Optional.of((V) map.remove(key));
    }
    return Optional.empty();
  }

  private static Map<Object, Object> getMap(Object owner) {
    if (CACHE_MAP.containsKey(owner)) {
      return CACHE_MAP.get(owner);
    }
    Map<Object, Object> m = new WeakHashMap<>();
    CACHE_MAP.put(owner, m);
    return m;
  }
}
