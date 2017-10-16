package xdean.jex.extra.collection.sequence;

import static org.junit.Assert.*;
import io.reactivex.Flowable;

import org.junit.Test;

import xdean.jex.extra.collection.sequence.IntSequence;

public class IntSequenceTest {
  @Test
  public void testSimple() throws Exception {
    IntSequence is = IntSequence.create(0, 1);
    Flowable.fromIterable(() -> is)
        .test(5)
        .assertValues(0, 1, 2, 3, 4);
  }

  @Test
  public void testRelease() throws Exception {
    IntSequence is = IntSequence.create(0, 1);
    assertEquals(0, is.next().intValue());
    assertEquals(1, is.next().intValue());
    assertEquals(2, is.next().intValue());
    assertEquals(3, is.next().intValue());
    is.release(0);
    is.release(3);
    assertEquals(0, is.next().intValue());
    assertEquals(3, is.next().intValue());
    assertEquals(4, is.next().intValue());
  }

  @Test
  public void testReleaseFail() throws Exception {
    IntSequence is = IntSequence.create(0, 1);
    assertEquals(0, is.next().intValue());
    assertEquals(1, is.next().intValue());
    assertEquals(2, is.next().intValue());
    assertEquals(3, is.next().intValue());
    assertTrue(is.release(3));
    assertFalse(is.release(5));
  }
}
