package xdean.jex.util.string;

import java.util.Base64;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

import com.google.common.collect.Ordering;

import xdean.jex.extra.collection.IntList;
import xdean.jex.util.cache.CacheUtil;

public class StringUtil {
  public static String replaceExcept(String origin, String from, String to, String except) {
    if (except.contains(from)) {
      String holder = notExistChars(origin).next().toString();
      return origin.replace(except, holder).replace(from, to).replace(holder, except);
    } else {
      return origin.replace(from, to);
    }
  }

  /**
   * Get a list of not exist chars in given string. You can use these chars as placeholder to
   * replace string safety.
   *
   * @param s
   * @return
   */
  public static Iterator<Character> notExistChars(String s) {
    return new Iterator<Character>() {
      IntList collect = IntList.create(s.chars().distinct().sorted().toArray());
      int current = 128;
      Character next = null;

      @Override
      public Character next() {
        if (calcNext()) {
          Character c = next;
          next = null;
          return c;
        } else {
          throw new NoSuchElementException("No next non-exist character.");
        }
      }

      @Override
      public boolean hasNext() {
        return calcNext();
      }

      private boolean calcNext() {
        if (next == null) {
          while (++current < Integer.MAX_VALUE) {
            if (collect.remove(current) == false) {
              next = (char) current;
              return true;
            }
          }
          return false;
        }
        return true;
      }
    };
  }

  public static String repeat(String st, int times) {
    StringBuilder sb = new StringBuilder();
    while (times-- > 0) {
      sb.append(st);
    }
    return sb.toString();
  }

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
    if (from > to || from < 0 || to > sourceText.length()) {
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

  /**
   * Get the first balance pair substring index.
   *
   * @param str the string
   * @param left left symbol
   * @param right right symbol
   * @return (leftIndex, rightIndex) If not found, rightIndex is -1.
   */
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

  public static int indexOfIgnoreCase(String str, String target) {
    return indexOfIgnoreCase(str, target, 0);
  }

  public static int indexOfIgnoreCase(String str, String target, int startIndex) {
    return CacheUtil.cache(str, "lowerCase", () -> str.toLowerCase())
        .indexOf(CacheUtil.cache(target, "lowerCase", () -> target.toLowerCase()), startIndex);
  }

  public static String encode(String str) {
    return new String(Base64.getEncoder().encode(str.getBytes()));
  }

  public static String decode(String str) {
    return new String(Base64.getDecoder().decode(str.getBytes()));
  }

  public static String camelToUnderline(String param) {
    if (param == null || "".equals(param.trim())) {
      return "";
    }
    int len = param.length();
    StringBuilder sb = new StringBuilder(len);
    for (int i = 0; i < len; i++) {
      char c = param.charAt(i);
      if (Character.isUpperCase(c) || Character.isDigit(c)) {
        sb.append("_");
        sb.append(c);
      } else {
        sb.append(Character.toUpperCase(c));
      }
    }
    return sb.toString();
  }
}
