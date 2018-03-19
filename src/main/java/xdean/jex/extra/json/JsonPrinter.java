package xdean.jex.extra.json;

import static xdean.jex.util.lang.ExceptionUtil.uncheck;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;

import com.google.common.base.Charsets;
import com.google.common.collect.Multimap;
import com.google.common.collect.Table;

import xdean.jex.extra.collection.Either;
import xdean.jex.extra.collection.Pair;
import xdean.jex.util.lang.PrimitiveTypeUtil;
import xdean.jex.util.reflect.ReflectUtil;

public class JsonPrinter {
  @SuppressWarnings("unchecked")
  public static JsonPrinter getDefault() {
    return getJava()
        .printIdPolicy(PrintIdPolicy.ONLY_REFERENCED)
        .addObjectClassHandler(Either.class, e -> e.unify(a -> a, b -> b))
        .addObjectClassHandler(Multimap.class, m -> m.asMap())
        .addObjectClassHandler(Table.class, t -> t.rowMap());
  }

  public static JsonPrinter getJava() {
    return new JsonPrinter()
        .addJavaHandlers()
        .filterTransient()
        .printClass(false)
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

  public enum PrintIdPolicy {
    /**
     * Print all object's id and use id to resolve reference.
     */
    ALL,
    /**
     * Use id to resolve reference and only print referenced object's id.
     */
    ONLY_REFERENCED,
    /**
     * Don't resolve reference, always print whole object.
     */
    OFF
  }

  private final List<Pair<Predicate<Object>, Function<Object, Object>>> objectHandlers = new LinkedList<>();
  private final List<Predicate<Field>> fieldFilters = new LinkedList<>();
  private String tabCharacter = "  ";
  private boolean printClass = false;
  private PrintIdPolicy idPolicy = PrintIdPolicy.OFF;
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

  public JsonPrinter printIdPolicy(PrintIdPolicy b) {
    idPolicy = b;
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
        .addObjectClassHandler(Path.class, p -> p.toString().replace('\\', '/'))
        .addObjectClassHandler(File.class, f -> f.toString().replace('\\', '/'));
  }

  public JsonPrinter filterTransient() {
    return this.addFieldFilter(f -> !Modifier.isTransient(f.getModifiers()));
  }

  public void print(Object o, PrintStream ps) {
    InnerPrinter innerPrinter = new InnerPrinter(ps);
    if (idPolicy == PrintIdPolicy.ONLY_REFERENCED) {
      innerPrinter.prepareReferenced(o);
    }
    innerPrinter.print(o);
  }

  public void println(Object o, PrintStream ps) {
    print(o, ps);
    ps.println();
  }

  public String toString(Object o, Charset cs) throws UnsupportedEncodingException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    print(o, new PrintStream(baos, false, cs.name()));
    return baos.toString(cs.name());
  }

  public String toString(Object o) {
    return uncheck(() -> toString(o, Charsets.UTF_8));
  }

  private class InnerPrinter {
    int tab;
    Map<Object, Void> referenced = new IdentityHashMap<>();
    Map<Object, Void> visited = new IdentityHashMap<>();
    InnerPrintStream out;

    InnerPrinter(PrintStream ps) {
      out = new InnerPrintStream(ps);
    }

    private InnerPrinter prepareReferenced(Object o) {
      traversal(o, new IdentityHashMap<>());
      return this;
    }

    private void traversal(Object o, Map<Object, Object> visited) {
      if (o == null) {
        return;
      }
      Class<?> clz = o.getClass();
      if (PrimitiveTypeUtil.isWrapper(clz)) {
      } else if (clz == String.class) {
      } else if (clz.isArray()) {
        int length = Array.getLength(o);
        for (int i = 0; i < length; i++) {
          traversal(Array.get(o, i), visited);
        }
      } else if (printStructedMap && Map.class.isAssignableFrom(clz)) {
        ((Map<?, ?>) o).forEach((k, v) -> {
          traversal(k, visited);
          traversal(v, visited);
        });
      } else if (printStructedList && Collection.class.isAssignableFrom(clz)) {
        ((Collection<?>) o).forEach(i -> traversal(i, visited));
      } else {
        Optional<Function<Object, Object>> selector = objectHandlers.stream()
            .filter(p -> p.getLeft().test(o))
            .map(Pair::getRight)
            .findFirst();
        if (selector.isPresent()) {
          traversal(selector.get().apply(o), visited);
        } else {
          if (visited.put(o, o) != null) {
            referenced.put(o, null);
            return;
          }
          Field[] allFields = ReflectUtil.getAllFields(o.getClass(), false);
          for (Field f : allFields) {
            if (!fieldFilters.stream().allMatch(p -> p.test(f))) {
              continue;
            }
            f.setAccessible(true);
            Object v = uncheck(() -> f.get(o));
            traversal(v, visited);
          }
        }
      }
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
        Optional<Function<Object, Object>> selector = objectHandlers.stream()
            .filter(p -> p.getLeft().test(o))
            .map(Pair::getRight)
            .findFirst();
        if (selector.isPresent()) {
          print(selector.get().apply(o));
        } else if ((idPolicy == PrintIdPolicy.ALL && visited.containsKey(o)) ||
            (idPolicy == PrintIdPolicy.ONLY_REFERENCED && visited.containsKey(o) && referenced.containsKey(o))) {
          print("@" + idFunction.apply(o));
        } else {
          visited.put(o, null);
          printObject(o);
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
      if (idPolicy == PrintIdPolicy.ALL || (idPolicy == PrintIdPolicy.ONLY_REFERENCED && referenced.containsKey(o))) {
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
