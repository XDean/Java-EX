package xdean.jex.util.security;

import java.io.IOException;
import java.io.InputStream;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import com.google.common.base.Stopwatch;

public class SecurityUtil {

  public static String md5(InputStream input) throws IOException {
    try {
      return digest(input, "MD5");
    } catch (NoSuchAlgorithmException e) {
      // MD5 must be ok
      throw new RuntimeException(e);
    }
  }

  public static String digest(InputStream input, String algorithm) throws IOException, NoSuchAlgorithmException {
    Stopwatch sw = Stopwatch.createStarted();
    int bufferSize = 256 * 1024;
    MessageDigest messageDigest = MessageDigest.getInstance(algorithm);
    try (DigestInputStream digestInputStream = new DigestInputStream(input, messageDigest);) {
      byte[] buffer = new byte[bufferSize];
      while (digestInputStream.read(buffer) > 0) {
        ;
      }
      messageDigest = digestInputStream.getMessageDigest();
      byte[] resultByteArray = messageDigest.digest();
      return byteArrayToHex(resultByteArray);
    } finally {
      sw.stop();
    }
  }

  private static String byteArrayToHex(byte[] b) {
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
