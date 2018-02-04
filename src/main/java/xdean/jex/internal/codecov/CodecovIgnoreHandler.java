package xdean.jex.internal.codecov;

import static xdean.jex.util.lang.ExceptionUtil.*;
import static xdean.jex.util.log.LogUtil.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.google.common.base.Charsets;

import xdean.jex.util.file.FileUtil;

/**
 *
 * @author XDean
 */
@CodecovIgnore
public class CodecovIgnoreHandler {
  private static final String END = "#end";
  private static final String IGNORE = "ignore:";
  private static final String GENERATED = "#generated";

  public static void main(String[] args) {
    updateCodecovIgnore();
  }

  public static void updateCodecovIgnore() {
    Path codecov = Paths.get("codecov.yml");
    if (!Files.exists(codecov)) {
      error().log("Can't find codecov.yml");
      return;
    }
    Path path = Paths.get("src", "main", "java");
    updateCodecovIgnore(codecov, path);
  }

  public static void updateCodecovIgnore(Path codecov, Path sourcePath) {
    FileUtil.deepTraversal(sourcePath)
        .filter(p -> !Files.isDirectory(p))
        .filter(p -> p.getFileName().toString().endsWith(".java"))
        .filter(p -> {
          String name = StreamSupport.stream(p.spliterator(), false)
              .skip(3)
              .map(Path::toString)
              .collect(Collectors.joining("."));
          String clzName = name.substring(0, name.length() - 5);
          return uncatch(() -> Class.forName(clzName).getAnnotation(CodecovIgnore.class)) != null;
        })
        .doOnNext(p -> debug().log("Find file to ignore: " + p))
        .toList()
        .subscribe(ignores -> writeIgnore(codecov, ignores));
  }

  private static void writeIgnore(Path codecov, List<Path> ignores) {
    List<String> lines = uncheck(() -> Files.readAllLines(codecov, Charsets.UTF_8));
    List<String> ignoreLines = ignores.stream()
        .map(p -> p.toString().replace('\\', '/'))
        .map(s -> "  - \"" + s + "\"")
        .collect(Collectors.toList());
    ignoreLines.add(0, GENERATED);
    ignoreLines.add(END);
    int ignoreLine = lines.indexOf(IGNORE);
    int generateLine = lines.indexOf(GENERATED);
    int endLine = lines.indexOf(END);
    if (ignoreLine == -1) {
      lines.add(IGNORE);
      lines.addAll(ignoreLines);
    } else {
      if (generateLine == -1 ^ endLine == -1) {
        error().log("'#generated' and '#end' tags are not synchronized, correct the file manually.");
        return;
      } else if (generateLine == -1) {
        lines.addAll(ignoreLine + 1, ignoreLines);
      } else {
        for (int i = generateLine; i <= endLine; i++) {
          lines.remove(generateLine);
        }
        lines.addAll(generateLine, ignoreLines);
      }
    }
    uncheck(() -> Files.write(codecov, lines));
    info().log("codecov.yml has been updated!");
  }
}
