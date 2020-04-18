package cn.xdean.jex.extra.json;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Test;

import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableMap;

public class JsonPrinterTest {
  @Test
  public void test() throws Exception {
    A a1 = new A(1);
    A a2 = new A(2);
    B b = new B("a", "b", "c");
    C c1 = new C(ImmutableMap.of("aa1", a1, "aa2", a2), b);
    C c2 = new C(ImmutableMap.of("aa1", a1, "aa2", a2), b);
    D d = new D(c1, Arrays.asList(c1, c2));
    assertWith(JsonPrinter.getDefault().toString(d), "onlyReferenced");
    assertWith(JsonPrinter.getDefault().printIdPolicy(JsonPrinter.PrintIdPolicy.ALL).toString(d), "allId");
    assertWith(JsonPrinter.getDefault().printIdPolicy(JsonPrinter.PrintIdPolicy.OFF).toString(d), "noId");
  }

  private void assertWith(String s, String file) throws IOException, URISyntaxException {
    Path p = Paths.get("src/test/resources/xdean/jex/extra/json/");
    Path goldenFile = p.resolve(file + ".json");
    if (!Files.exists(goldenFile)) {
      Files.createDirectories(goldenFile.getParent());
      Files.createFile(goldenFile);
      OutputStream output = Files.newOutputStream(goldenFile);
      output.write(s.getBytes(Charsets.UTF_8));
      output.flush();
      output.close();
      return;
    }
    List<String> lines = Files.readAllLines(goldenFile, Charsets.UTF_8);
    assertEquals(lines.stream().collect(Collectors.joining(System.lineSeparator())), s);
  }

  static class A {
    int i;

    public A(int i) {
      this.i = i;
    }
  }

  static class B {
    String[] strs;

    public B(String... strs) {
      this.strs = strs;
    }
  }

  static class C {
    Map<String, A> map;
    B b;

    public C(Map<String, A> map, B b) {
      super();
      this.map = map;
      this.b = b;
    }
  }

  static class D {
    transient C t;
    List<C> list;

    public D(C t, List<C> list) {
      this.t = t;
      this.list = list;
    }
  }
}
