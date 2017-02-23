package xdean.jex.util.collection;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Assert;
import org.junit.Test;

public class TestTraversalUtil {

  @Test
  public void testDeepTraversal() {
    TraversalUtil.deepTraversal(1, i -> i < 10 ? Arrays.asList(i * 2, i * 2 + 1) : Collections.emptyList())
        .toList()
        .subscribe(l -> Assert.assertEquals(
            Arrays.asList(1, 2, 4, 8, 16, 17, 9, 18, 19, 5, 10, 11, 3, 6, 12, 13, 7, 14, 15), l));
  }

  @Test
  public void testWideTraversal() {
    TraversalUtil.wideTraversal(1, i -> i < 10 ? Arrays.asList(i * 2, i * 2 + 1) : Collections.emptyList())
        .toList()
        .subscribe(l -> Assert.assertEquals(
            Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), l));
  }
}
