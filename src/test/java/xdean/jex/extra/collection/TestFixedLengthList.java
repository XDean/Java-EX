package xdean.jex.extra.collection;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

public class TestFixedLengthList {
  @Test
  public void test() {
    List<Integer> l = new FixedLengthList<>(5);
    String answer = "";
    for (int i = 0; i < 10; i++) {
      l.add(i);
      answer += l.get(0);
    }
    Assert.assertEquals("0000012345", answer);
  }
}
