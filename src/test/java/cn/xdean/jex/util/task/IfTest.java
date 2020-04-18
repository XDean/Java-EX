package cn.xdean.jex.util.task;

import static org.junit.Assert.assertEquals;

import java.util.NoSuchElementException;

import org.junit.Test;

public class IfTest {
  @Test
  public void testNormal() {
    assertEquals(1, If.that(true).tobe(1).orbe(2).result());
    assertEquals(2, If.that(false).tobe(1).orbe(2).result());
  }

  @Test
  public void testSeries() {
    assertEquals(1, If.that(true).tobe(1).and(false).tobe(2).result());
    assertEquals(2, If.that(true).tobe(1).or(false).tobe(2).result());
    assertEquals(1, If.that(true).tobe(1).or(false).tobe(2).end().result());
    assertEquals(2, If.that(true).tobe(1).or(false).tobe(2).end(true).result());
  }

  @Test(expected = NoSuchElementException.class)
  public void testNoResult() {
    If.that(true).result();
  }

  @Test(expected = IllegalStateException.class)
  public void testEnd() {
    If.that(true).end();
  }
}
