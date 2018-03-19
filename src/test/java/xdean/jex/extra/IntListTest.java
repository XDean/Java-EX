package xdean.jex.extra;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import xdean.jex.extra.collection.IntList;

public class IntListTest {
  IntList list = IntList.create();

  @Test
  public void test() throws Exception {
    assertEquals("IntList: []", list.toString());
    assertEquals(0, list.size());
    list.add(1);
    list.add(0, 2);
    assertEquals(2, list.get(0));
    list.addAll(new int[] { 3, 4 });
    list.addAll(0, new int[] { 5, 6 });
    list.remove(1);
    list.removeIndex(2);
    list.removeAll(new int[] { 3, 5 });
    assertArrayEquals(new int[] { 6, 4 }, list.toArray());
    list.addAll(new int[] { 3, 4, 5 });
    list.set(0, 1);
    list.set(1, 2);
    assertArrayEquals(new int[] { 1, 2, 3, 4, 5 }, list.toArray());
    assertTrue(list.contains(3));
    assertFalse(list.contains(6));
    assertTrue(list.containsAll(new int[] { 1, 2, 5 }));
    list.retainAll(new int[] { 1, 3, 5 });
    assertArrayEquals(new int[] { 1, 3, 5 }, list.toArray());
    list.add(1);
    assertEquals(0, list.indexOf(1));
    assertEquals(3, list.lastIndexOf(1));
    assertEquals("IntList: [1, 3, 5, 1]", list.toString());
    list.sort();
    AtomicInteger count = new AtomicInteger();
    list.forEach(i -> count.addAndGet(i));
    assertEquals(10, count.get());
    assertArrayEquals(new int[] { 1, 1, 3, 5 }, list.toArray());
    list.clear();
    assertTrue(list.isEmpty());
  }

  @Test
  public void testSerialize() throws Exception {
    list.addAll(new int[] { 1, 2, 3 });
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(baos);
    oos.writeObject(list);
    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
    ObjectInputStream ois = new ObjectInputStream(bais);
    Object read = ois.readObject();
    assertEquals(list, read);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testOverflow() throws Exception {
    list.set(1, 1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testOverflow2() throws Exception {
    list.removeIndex(1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testOverflow3() throws Exception {
    list.add(1, 1);
  }
}
