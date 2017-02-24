package xdean.jex.util.file;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.concurrent.TimeUnit;

import com.google.common.base.Stopwatch;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import rx.Observable;
import xdean.jex.util.collection.TraversalUtil;
import xdean.jex.util.task.TaskUtil;

@Slf4j
@UtilityClass
public class FileUtil {

  public void createDirectory(Path path) throws IOException {
    if (Files.notExists(path)) {
      Files.createDirectory(path);
    }
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
    try {
      return digest(path, "MD5");
    } catch (NoSuchAlgorithmException e) {
      // MD5 must be ok
      throw new RuntimeException(e);
    }
  }

  public String digest(Path path, String algorithm) throws IOException, NoSuchAlgorithmException {
    log.debug("To calc {}'s {}, its size is: {}", path.getFileName(), algorithm, Files.size(path));
    Stopwatch sw = Stopwatch.createStarted();
    int bufferSize = 256 * 1024;
    MessageDigest messageDigest = MessageDigest.getInstance(algorithm);
    try (
        InputStream fileInputStream = Files.newInputStream(path);
        DigestInputStream digestInputStream = new DigestInputStream(fileInputStream, messageDigest);) {

      byte[] buffer = new byte[bufferSize];
      while (digestInputStream.read(buffer) > 0) {
        ;
      }
      messageDigest = digestInputStream.getMessageDigest();
      byte[] resultByteArray = messageDigest.digest();
      return byteArrayToHex(resultByteArray);
    } finally {
      sw.stop();
      log.debug("Calc end, use {} ms", sw.elapsed(TimeUnit.MILLISECONDS));
    }
  }

  private String byteArrayToHex(byte[] b) {
    String hs = "";
    String stmp = "";
    for (int n = 0; n < b.length; n++) {
      stmp = (Integer.toHexString(b[n] & 0XFF));
      if (stmp.length() == 1) {
        hs = hs + "0" + stmp;
      } else {
        hs = hs + stmp;
      }
      if (n < b.length - 1) {
        hs = hs + "";
      }
    }
    return hs;
  }
}
