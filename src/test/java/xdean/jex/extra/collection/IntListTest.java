package xdean.jex.extra.collection;

import static xdean.jex.extra.collection.IntList.create;

import java.util.Arrays;

import org.junit.Test;

public class IntListTest {
  @Test
  public void testConstructor() throws Exception {
    create();
    create(Arrays.asList(1, 2, 3));
    create(5);
    create(new int[] { 1, 2, 3 });
  }

}