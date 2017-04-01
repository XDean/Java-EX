package xdean.jex.util.cache;

import java.util.Map;
import java.util.Optional;
import java.util.WeakHashMap;
import java.util.function.Supplier;

import lombok.experimental.UtilityClass;
import xdean.jex.util.collection.MapUtil;

@UtilityClass
@SuppressWarnings("unchecked")
public class CacheUtil {

  private final Map<Class<?>, Map<Object, Map<Object, Object>>> CACHE_MAP = createMap();

  public <V> V cache(Object key, Supplier<V> factory) {
    return cache(key.getClass(), key, factory);
  }

  public <V> V cache(Object owner, Object key, Supplier<V> factory) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return (V) map.get(key);
    }
    V v = factory.get();
    map.put(key, v);
    return v;
  }

  public <V> Optional<V> get(Object key) {
    return get(key.getClass(), key);
  }

  public <V> Optional<V> get(Object owner, Object key) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return Optional.of((V) map.get(key));
    }
    return Optional.empty();
  }

  public <V> void set(Object key, V value) {
    set(key.getClass(), key, value);
  }

  public <V> void set(Object owner, Object key, V value) {
    Map<Object, Object> map = getMap(owner);
    map.put(key, value);
  }

  public <V> Optional<V> remove(Object key) {
    return remove(key.getClass(), key);
  }

  public <V> Optional<V> remove(Object owner, Object key) {
    Map<Object, Object> map = getMap(owner);
    if (map.containsKey(key)) {
      return Optional.of((V) map.remove(key));
    }
    return Optional.empty();
  }

  private Map<Object, Object> getMap(Object owner) {
    return MapUtil.getOrPutDefault(
        MapUtil.getOrPutDefault(
            CACHE_MAP,
            owner.getClass(),
            () -> createMap()),
        owner,
        () -> createMap());
  }

  private <K, V> Map<K, V> createMap() {
    return new WeakHashMap<>();// XXX:Synchronize?
  }
}
