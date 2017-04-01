package xdean.jex.util.file;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.concurrent.TimeUnit;
import java.util.function.UnaryOperator;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import rx.Observable;
import xdean.jex.util.collection.TraversalUtil;
import xdean.jex.util.security.SecurityUtil;
import xdean.jex.util.task.TaskUtil;

import com.google.common.base.Stopwatch;

@Slf4j
@UtilityClass
public class FileUtil {

  public String getNameWithoutSuffix(Path path) {
    String name = path.getFileName().toString();
    return name.substring(0, name.lastIndexOf('.'));
  }

  public void createDirectory(Path path) throws IOException {
    if (Files.notExists(path)) {
      Files.createDirectory(path);
    }
  }

  public Path rename(Path path, String name) throws IOException {
    return Files.move(path, path.resolveSibling(name));
  }

  public Path rename(Path path, UnaryOperator<String> nameConverter) throws IOException {
    return rename(path, nameConverter.apply(path.getFileName().toString()));
  }

  /**
   * Test if the both paths are same file
   * 
   * @param p1
   * @param p2
   * @return
   * @throws IOException
   */
  public boolean equals(Path p1, Path p2) throws IOException {
    if (Files.size(p1) != Files.size(p2)) {
      return false;
    }
    try (InputStream is1 = Files.newInputStream(p1);
        InputStream is2 = Files.newInputStream(p2)) {
      int c;
      while ((c = is1.read()) != -1) {
        if (is2.read() != c) {
          return false;
        }
      }
      return true;
    }
  }

  public Observable<Path> deepTraversal(Path path) {
    return TraversalUtil.deepTraversal(path,
        p -> TaskUtil.firstSuccess(() -> Files.newDirectoryStream(p), () -> Collections.<Path> emptyList()));
  }

  public Observable<Path> wideTraversal(Path path) {
    return TraversalUtil.wideTraversal(path,
        p -> TaskUtil.firstSuccess(() -> Files.newDirectoryStream(p), () -> Collections.<Path> emptyList()));
  }

  public String md5(Path path) throws IOException {
    return SecurityUtil.md5(Files.newInputStream(path));
  }

  public String digest(Path path, String algorithm) throws NoSuchAlgorithmException, IOException {
    log.debug("To calc {}'s {}, its size is: {}", path.getFileName(), algorithm, Files.size(path));
    Stopwatch s = Stopwatch.createStarted();
    try {
      return SecurityUtil.digest(Files.newInputStream(path), algorithm);
    } finally {
      log.debug("Calc end, use {} ms", s.elapsed(TimeUnit.MILLISECONDS));
    }
  }
}
