package cn.xdean.jex.lang;

import cn.xdean.jex.lang.collection.Wrapper;
import cn.xdean.jex.reflect.ReflectUtil;
import org.junit.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static cn.xdean.jex.lang.CacheUtil.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class CacheUtilTest {
  @Test
  public void testGetSet() throws Exception {
    Object key = key();
    set(key, 123);
    assertEquals(123, get(key).get());
  }

  @Test
  public void testCache() throws Exception {
    AtomicInteger call = new AtomicInteger(0);
    Object key = key();
    cache(key, () -> call.incrementAndGet());
    cache(key, () -> call.incrementAndGet());
    cache(key, () -> call.incrementAndGet());
    assertEquals(1, call.get());
  }

  @Test
  public void testCacheGC() throws Exception {
    AtomicInteger call = new AtomicInteger(0);
    Object key = key();
    cache(key, () -> call.incrementAndGet());
    key = null;
    System.gc();
    key = key();
    cache(key, () -> call.incrementAndGet());
    cache(key, () -> call.incrementAndGet());
    key = null;
    System.gc();
    key = key();
    cache(key, () -> call.incrementAndGet());
    assertEquals(3, call.get());
  }

  @Test
  public void testCacheWeak() throws Exception {
    AtomicInteger call = new AtomicInteger(0);
    Object key = key();
    cacheWeak(key, () -> call.incrementAndGet());
    cacheWeak(key, () -> call.incrementAndGet());
    cacheWeak(key, () -> call.incrementAndGet());
    assertEquals(1, call.get());
  }

  @Test
  public void testCacheWeakGC() throws Exception {
    AtomicInteger call = new AtomicInteger(1000);
    Object k = key();
    cacheWeak(k, () -> call.incrementAndGet());
    System.gc();
    cacheWeak(k, () -> call.incrementAndGet());
    cacheWeak(k, () -> call.incrementAndGet());
    System.gc();
    cacheWeak(k, () -> call.incrementAndGet());
    assertEquals(1003, call.get());
  }

  @Test
  public void testRemove() throws Exception {
    Object key = key();
    set(key, 123);
    assertEquals(123, remove(key).get());
    assertFalse(remove(new Object()).isPresent());
  }

  private Object key() {
    return new Wrapper<>(ReflectUtil.getCaller(1, false).getMethodName());
  }
}
