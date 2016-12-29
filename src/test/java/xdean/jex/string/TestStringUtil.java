package xdean.jex.string;

import org.junit.Assert;
import org.junit.Test;

import xdean.jex.util.string.StringUtil;

public class TestStringUtil {

  @Test
  public void firstIndexOf() {
    int firstIndexOf = StringUtil.firstIndexOf(
        "linear-gradient(from 0px 0px to 0px 4px, derive(red, -4%), derive(yellow, 10%))",
        "derive", "ladder");
    Assert.assertEquals(41, firstIndexOf);
  }

  @Test
  public void balancePair() {
    String str = "a{b{c}d}{e}f{g}h}i}";
    int[] pair = StringUtil.balancePair(str, "{", "}");
    Assert.assertEquals("{b{c}d}", str.substring(pair[0], pair[1] + 1));

    str = "a{b{c{d{e}f}g}";
    pair = StringUtil.balancePair(str, "{", "}");
    Assert.assertEquals(1, pair[0]);
    Assert.assertEquals(-1, pair[1]);
  }
}
