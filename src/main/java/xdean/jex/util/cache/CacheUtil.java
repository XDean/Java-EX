package xdean.jex.util.cache;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import com.google.common.collect.MapMaker;

import xdean.jex.util.collection.MapUtil;

@SuppressWarnings("unchecked")
public class CacheUtil {

  private static final Map<Class<?>, Map<Object, Map<Object, Object>>> CACHE_MAP = createMap();

  public static <V> V cacheWeak(Object key, Supplier<V> factory) {
    return cacheWeak(key.getClass(), key, factory);
  }

  public static <V> V cacheWeak(Object owner, Object key, Supplier<V> factory) {
    WeakReference<V> ref = cache(owner, key, () -> new WeakReference<>(factory.get()));
    V v = ref.get();
    if (v == null) {
      v = factory.get();
      set(owner, key, new WeakReference<>(v));
    }
    return v;
  }

  public static <V> V cache(Object key, Supplier<V> factory) {
    return cache(key.getClass(), key, factory);
  }

  public static <V> V cache(Object owner, Object key, Supplier<V> factory) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return (V) map.get(key);
    }
    V v = factory.get();
    map.put(key, v);
    return v;
  }

  public static <V> Optional<V> get(Object key) {
    return get(key.getClass(), key);
  }

  public static <V> Optional<V> get(Object owner, Object key) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return Optional.of((V) map.get(key));
    }
    return Optional.empty();
  }

  public static <V> void set(Object key, V value) {
    set(key.getClass(), key, value);
  }

  public static <V> void set(Object owner, Object key, V value) {
    Map<Object, Object> map = getMap(owner);
    map.put(key, value);
  }

  public static <V> Optional<V> remove(Object key) {
    return remove(key.getClass(), key);
  }

  public static <V> Optional<V> remove(Object owner, Object key) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return Optional.of((V) map.remove(key));
    }
    return Optional.empty();
  }

  private static Map<Object, Object> getMap(Object owner) {
    return MapUtil.getOrPutDefault(
        MapUtil.getOrPutDefault(
            CACHE_MAP,
            owner.getClass(),
            () -> createMap()),
        owner,
        () -> createMap());
  }

  private static <K, V> Map<K, V> createMap() {
    return new MapMaker().weakKeys().makeMap();// XXX:Synchronize?
  }

  public Map<Class<?>, Map<Object, Map<Object, Object>>> getAllCache() {
    return Collections.unmodifiableMap(CACHE_MAP);
  }
}
