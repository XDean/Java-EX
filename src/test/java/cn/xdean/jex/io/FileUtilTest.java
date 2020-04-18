package cn.xdean.jex.io;

import cn.xdean.jex.lang.ExceptionUtil;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class FileUtilTest {
  private static final URI CP = ExceptionUtil.uncheck(() -> FileUtilTest.class.getResource(".").toURI());

  @Test
  public void testDeepTraversal() {
    FileUtil.deepTraversal(Paths.get(CP).resolve("count"))
        .map(p -> p.getFileName().toString())
        .test()
        .assertValueCount(6);
  }

  @Test
  public void testWideTraversal() {
    FileUtil.wideTraversal(Paths.get(CP).resolve("count"))
        .test()
        .assertValueCount(6);
  }

  @Test
  public void testEquals() throws IOException {
    Assert.assertTrue(FileUtil.equals(
        Paths.get(CP).resolve("file1"),
        Paths.get(CP).resolve("file1_copy")));
  }

  @Test
  public void testNotEquals() throws IOException {
    Assert.assertFalse(FileUtil.equals(
        Paths.get(CP).resolve("file1"),
        Paths.get(CP).resolve("file2")));
  }

  @Test
  public void testMd5() throws IOException {
    assertEquals(
        "01e8f91c94041493aaddc6dee5aeffc7",
        FileUtil.md5(Paths.get(CP).resolve("md5")));
  }

  @Test
  public void testGetNameWithoutSuffix() throws Exception {
    assertEquals("file", FileUtil.getNameWithoutSuffix(Paths.get(CP).resolve("file.txt")));
    assertEquals("file1", FileUtil.getNameWithoutSuffix(Paths.get(CP).resolve("file1")));
  }
}
