package cn.xdean.jex.lang;

import io.reactivex.Flowable;
import org.junit.Test;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.experimental.theories.suppliers.TestedOn;
import org.junit.runner.RunWith;

import java.util.stream.IntStream;

import static cn.xdean.jex.lang.StringUtil.*;
import static org.junit.Assert.assertEquals;

@RunWith(Theories.class)
public class StringUtilTest {

  @Test
  public void testReplaceExcept() throws Exception {
    assertEquals("//\\n\\n////n", replaceExcept("///n/n////n", "/", "\\", "//"));
  }

  @Test
  public void testFirstIndexOf() {
    int firstIndexOf = firstIndexOf(
        "linear-gradient(from 0px 0px to 0px 4px, derive(red, -4%), derive(yellow, 10%))",
        "derive", "ladder");
    assertEquals(41, firstIndexOf);
  }

  @Test
  public void testBalancePair() {
    String str = "a{b{c}d}{e}f{g}h}i}";
    int[] pair = balancePair(str, "{", "}");
    assertEquals("{b{c}d}", str.substring(pair[0], pair[1] + 1));

    str = "a{b{c{d{e}f}g}";
    pair = balancePair(str, "{", "}");
    assertEquals(1, pair[0]);
    assertEquals(-1, pair[1]);
  }

  @Theory
  public void testNotExistChars(@TestedOn(ints = { 5, 50, 500 }) int count) {
    StringBuilder sb = new StringBuilder();
    IntStream.range(0, count)
        .mapToObj(a -> (char) (Math.random() * 100 + 128))
        .forEach(sb::append);
    String string = sb.toString();
    Flowable.fromIterable(() -> notExistChars(string))
        .test(0)
        .requestMore(count)
        .assertNever(c -> string.indexOf(c.charValue()) != -1);
  }
}
