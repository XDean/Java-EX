package xdean.jex.util.collection;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Test;

public class TestTraversalUtil {

  @Test
  public void testPreOrderTraversal() throws Exception {
    TraversalUtil.preOrderTraversal(1, i -> i < 10 ? Arrays.asList(i * 2, i * 2 + 1) : Collections.emptyList())
        .test()
        .assertValues(1, 2, 4, 8, 16, 17, 9, 18, 19, 5, 10, 11, 3, 6, 12, 13, 7, 14, 15);
  }

  @Test
  public void testPostOrderTraversal() throws Exception {
    TraversalUtil.postOrderTraversal(1, i -> i < 10 ? Arrays.asList(i * 2, i * 2 + 1) : Collections.emptyList())
        .test()
        .assertValues(16, 17, 8, 18, 19, 9, 4, 10, 11, 5, 2, 12, 13, 6, 14, 15, 7, 3, 1);
  }

  @Test
  public void testBreadthFirstTraversal() throws Exception {
    TraversalUtil.breadthFirstTraversal(1, i -> i < 10 ? Arrays.asList(i * 2, i * 2 + 1) : Collections.emptyList())
        .test()
        .assertValues(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19);
  }
}
