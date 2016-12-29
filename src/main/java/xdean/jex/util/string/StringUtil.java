package xdean.jex.util.string;

import java.util.Base64;
import java.util.stream.Stream;

import com.google.common.collect.Ordering;

public class StringUtil {

  public static int firstIndexOf(String str, String... ts) {
    return Stream.of(ts)
        .map(str::indexOf)
        .filter(i -> i != -1)
        .min(Ordering.natural())
        .orElse(-1);
  }

  public static int lastIndexOf(String str, String... ts) {
    return Stream.of(ts)
        .map(str::lastIndexOf)
        .filter(i -> i != -1)
        .max(Ordering.natural())
        .orElse(-1);
  }

  public static String replacePart(String sourceText, int from, int to, String replaceText) {
    if (from < to || from < 0 || to > sourceText.length()) {
      throw new StringIndexOutOfBoundsException();
    }
    return String.format("%s%s%s", sourceText.substring(0, from), replaceText, sourceText.substring(to));
  }

  public static String unWrap(String str, String left, String right) {
    String trim = str.trim();
    if (trim.length() < left.length() + right.length()) {
      return str;
    }
    if (trim.startsWith(left) && trim.endsWith(right)) {
      return trim.substring(left.length(), trim.length() - right.length());
    } else {
      return str;
    }
  }

  public static int[] balancePair(String str, String left, String right) {
    int count = 0;
    int offset = 0;
    int firstLeft = -1;
    while (true) {
      int leftIndex = str.indexOf(left, offset);
      int rightIndex = str.indexOf(right, offset);
      if (leftIndex == rightIndex) {
        return new int[] { firstLeft, -1 };
      } else if ((leftIndex < rightIndex && leftIndex != -1) || rightIndex == -1) {
        if (firstLeft == -1) {
          firstLeft = leftIndex;
        }
        count++;
        offset = leftIndex + 1;
      } else {
        count--;
        offset = rightIndex + 1;
        if (count < 0) {
          return new int[] { firstLeft, -1 };
        }
        if (count == 0) {
          return new int[] { firstLeft, rightIndex };
        }
      }
    }
  }

  public static boolean isControlCharacter(char c) {
    return c < 32 || c == 127;
  }

  public static int countLine(String str) {
    return str.split("\\R").length;
  }

  public static boolean isEmpty(String str) {
    return str == null || str.equals("");
  }

  public static boolean notEmpty(String str) {
    return !isEmpty(str);
  }

  public static String upperFirst(String st) {
    return st.substring(0, 1).toUpperCase() + st.substring(1).toLowerCase();
  }

  public static String encode(String str) {
    return new String(Base64.getEncoder().encode(str.getBytes()));
  }

  public static String decode(String str) {
    return new String(Base64.getDecoder().decode(str.getBytes()));
  }
}
