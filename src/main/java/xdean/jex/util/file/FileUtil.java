package xdean.jex.util.file;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileUtil {
  public static void createDirectory(Path path) throws IOException {
    if (Files.notExists(path)) {
      Files.createDirectory(path);
    }
  }
}
