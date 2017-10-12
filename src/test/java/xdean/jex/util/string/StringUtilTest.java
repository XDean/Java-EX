package xdean.jex.util.string;

import io.reactivex.Flowable;

import java.util.stream.IntStream;

import org.junit.Assert;
import org.junit.Test;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.experimental.theories.suppliers.TestedOn;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class StringUtilTest {

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

  @Theory
  public void notExistChars(@TestedOn(ints = { 5, 50, 500 }) int count) {
    StringBuilder sb = new StringBuilder();
    IntStream.range(0, count)
        .mapToObj(a -> (char) (Math.random() * 100 + 128))
        .forEach(sb::append);
    String string = sb.toString();
    Flowable.fromIterable(() -> StringUtil.notExistChars(string))
        .test(0)
        .requestMore(count)
        .assertNever(c -> string.indexOf(c.charValue()) != -1);
  }
}
