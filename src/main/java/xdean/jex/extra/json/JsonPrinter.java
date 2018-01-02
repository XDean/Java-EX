package xdean.jex.extra.json;

import static xdean.jex.util.lang.ExceptionUtil.uncheck;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;

import com.google.common.collect.Multimap;
import com.google.common.collect.Table;

import xdean.jex.extra.Either;
import xdean.jex.extra.Pair;
import xdean.jex.util.lang.PrimitiveTypeUtil;
import xdean.jex.util.reflect.ReflectUtil;

public class JsonPrinter {
  @SuppressWarnings("unchecked")
  public static JsonPrinter getDefault() {
    return getJava()
        .addObjectClassHandler(Either.class, e -> e.unify(a -> a, b -> b))
        .addObjectClassHandler(Multimap.class, m -> m.asMap())
        .addObjectClassHandler(Table.class, t -> t.rowMap());
  }

  public static JsonPrinter getJava() {
    return new JsonPrinter()
        .addJavaHandlers()
        .filterTransient()
        .printId(true)
        .printClass(true)
        .printStructedList(true)
        .printStructedMap(true)
        .idFunction(autoIncreaseId(0));
  }

  public static JsonPrinter getEmpty() {
    return new JsonPrinter();
  }

  public static Function<Object, Integer> autoIncreaseId(int from) {
    AtomicInteger id = new AtomicInteger(from);
    Map<Object, Integer> map = new IdentityHashMap<>();
    return o -> map.computeIfAbsent(o, e -> id.getAndIncrement());
  }

  private final List<Pair<Predicate<Object>, Function<Object, Object>>> objectHandlers = new LinkedList<>();
  private final List<Predicate<Field>> fieldFilters = new LinkedList<>();
  private String tabCharacter = "  ";
  private boolean printClass = false;
  private boolean printId = false;
  private boolean printStructedList = false;
  private boolean printStructedMap = false;
  private Function<Object, Integer> idFunction = System::identityHashCode;

  @SuppressWarnings("unchecked")
  public <T> JsonPrinter addObjectClassHandler(Class<T> clz, Function<T, Object> f) {
    return addObjectHandler(o -> clz.isInstance(o), o -> f.apply((T) o));
  }

  public JsonPrinter addObjectHandler(Predicate<Object> p, Function<Object, Object> f) {
    objectHandlers.add(Pair.of(p, f));
    return this;
  }

  public JsonPrinter addFieldFilter(Predicate<Field> p) {
    fieldFilters.add(p);
    return this;
  }

  public JsonPrinter tab(String t) {
    tabCharacter = t;
    return this;
  }

  public JsonPrinter printId(boolean b) {
    printId = b;
    return this;
  }

  public JsonPrinter printClass(boolean b) {
    printClass = b;
    return this;
  }

  public JsonPrinter printStructedList(boolean b) {
    printStructedList = b;
    return this;
  }

  public JsonPrinter printStructedMap(boolean b) {
    printStructedMap = b;
    return this;
  }

  public JsonPrinter idFunction(Function<Object, Integer> idFunction) {
    this.idFunction = idFunction;
    return this;
  }

  @SuppressWarnings("unchecked")
  public JsonPrinter addJavaHandlers() {
    return this
        .addObjectClassHandler(Enum.class, e -> e.name())
        .addObjectClassHandler(Optional.class, o -> o.orElse(null))
        .addObjectClassHandler(OptionalInt.class, i -> i.isPresent() ? i.getAsInt() : null)
        .addObjectClassHandler(OptionalDouble.class, i -> i.isPresent() ? i.getAsDouble() : null)
        .addObjectClassHandler(OptionalLong.class, i -> i.isPresent() ? i.getAsLong() : null)
        .addObjectClassHandler(Class.class, c -> c.getName())
        .addObjectClassHandler(Path.class, p -> p.toString())
        .addObjectClassHandler(File.class, f -> f.toString());
  }

  public JsonPrinter filterTransient() {
    return this.addFieldFilter(f -> !Modifier.isTransient(f.getModifiers()));
  }

  public void print(Object o, PrintStream ps) {
    new InnerPrinter(ps).print(o);
  }

  public void println(Object o, PrintStream ps) {
    print(o, ps);
    ps.println();
  }

  public String toString(Object o) {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    print(o, new PrintStream(baos));
    return baos.toString();
  }

  private class InnerPrinter {
    int tab;
    Set<Integer> visited = new HashSet<>();
    InnerPrintStream out;

    InnerPrinter(PrintStream ps) {
      out = new InnerPrintStream(ps);
    }

    void print(Object o) {
      if (o == null) {
        out.print(o);
        return;
      }
      Class<?> clz = o.getClass();
      if (PrimitiveTypeUtil.isWrapper(clz)) {
        out.printPrimitive(o);
      } else if (clz == String.class) {
        out.print(wrap((String) o));
      } else if (clz.isArray()) {
        int length = Array.getLength(o);
        List<Object> l = new ArrayList<>(length);
        for (int i = 0; i < length; i++) {
          l.add(Array.get(o, i));
        }
        printList(l);
      } else if (printStructedMap && Map.class.isAssignableFrom(clz)) {
        printMap((Map<?, ?>) o);
      } else if (printStructedList && Collection.class.isAssignableFrom(clz)) {
        printList((Collection<?>) o);
      } else {
        Optional<Function<Object, Object>> toString = objectHandlers.stream()
            .filter(p -> p.getLeft().test(o))
            .map(Pair::getRight)
            .findFirst();
        if (toString.isPresent()) {
          print(toString.get().apply(o));
        } else {
          Integer id = idFunction.apply(o);
          if (visited.add(id)) {
            printObject(o);
          } else {
            print("@" + id);
          }
        }
      }
    }

    void printList(Collection<?> l) {
      Iterator<?> i = l.iterator();
      if (i.hasNext()) {
        out.println('[');
        tab++;
        while (true) {
          out.printTab();
          print(i.next());
          if (i.hasNext()) {
            out.println(',');
          } else {
            out.println();
            break;
          }
        }
        tab--;
        out.printTab();
        out.print(']');
      } else {
        out.print("[]");
      }
    }

    <K, V> void printMap(Map<K, V> map) {
      Iterator<Entry<K, V>> i = map.entrySet().iterator();
      if (i.hasNext()) {
        out.println('{');
        tab++;
        while (true) {
          out.printTab();
          Entry<K, V> next = i.next();
          print(next.getKey());
          out.print(": ");
          print(next.getValue());
          if (i.hasNext()) {
            out.println(',');
          } else {
            out.println();
            break;
          }
        }
        tab--;
        out.printTab();
        out.print('}');
      } else {
        out.print("{}");
      }
    }

    void printObject(Object o) {
      Field[] allFields = ReflectUtil.getAllFields(o.getClass(), false);
      Map<String, Object> map = new LinkedHashMap<>();
      if (printId) {
        map.put("UID", "@" + idFunction.apply(o));
      }
      if (printClass) {
        map.put("class", o.getClass());
      }
      for (Field f : allFields) {
        if (!fieldFilters.stream().allMatch(p -> p.test(f))) {
          continue;
        }
        f.setAccessible(true);
        Object v = uncheck(() -> f.get(o));
        map.put(f.getName(), v);
      }
      printMap(map);
    }

    class InnerPrintStream {
      private PrintStream ps;

      InnerPrintStream(PrintStream ps) {
        this.ps = ps;
      }

      void print(Object o) {
        ps.print(o);
      }

      void printPrimitive(Object o) {
        if (o instanceof Character) {
          print(Character.getNumericValue((Character) o));
        } else {
          print(o);
        }
      }

      void printTab() {
        for (int i = 0; i < tab; i++) {
          ps.print(tabCharacter);
        }
      }

      void println() {
        ps.println();
      }

      void println(Object o) {
        ps.println(o);
      }
    }
  }

  private static String wrap(String string) {
    char c = 0;
    int i;
    int len = string.length();
    StringBuilder sb = new StringBuilder(len + 4);
    String t;

    sb.append('"');
    for (i = 0; i < len; i += 1) {
      c = string.charAt(i);
      switch (c) {
      case '\\':
      case '"':
        sb.append('\\');
        sb.append(c);
        break;
      case '\b':
        sb.append("\\b");
        break;
      case '\t':
        sb.append("\\t");
        break;
      case '\n':
        sb.append("\\n");
        break;
      case '\f':
        sb.append("\\f");
        break;
      case '\r':
        sb.append("\\r");
        break;
      default:
        if (c < ' ') {
          t = "000" + Integer.toHexString(c);
          sb.append("\\u" + t.substring(t.length() - 4));
        } else {
          sb.append(c);
        }
      }
    }
    sb.append('"');
    return sb.toString();
  }
}
