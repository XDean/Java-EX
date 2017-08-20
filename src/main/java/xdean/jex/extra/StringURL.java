package xdean.jex.extra;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;

import xdean.jex.util.lang.ExceptionUtil;
import xdean.jex.util.string.StringUtil;

/**
 * An URL who return the input string
 * 
 * @author XDean
 *
 */
public class StringURL {

  static {
    URL.setURLStreamHandlerFactory(new StringURLStreamHandlerFactory());
  }

  public static String createURLString(String text) {
    return "string:" + StringUtil.encode(text);
  }

  public static URL createURL(String text) {
    return ExceptionUtil.uncheck(() -> new URL(createURLString(text)));
  }

  private static class StringURLConnection extends URLConnection {
    public StringURLConnection(URL url) {
      super(url);
    }

    @Override
    public void connect() throws IOException {
    }

    @Override
    public InputStream getInputStream() throws IOException {
      return new ByteArrayInputStream(StringUtil.decode(getURL().toString().substring(7)).getBytes());
    }
  }

  private static class StringURLStreamHandlerFactory implements URLStreamHandlerFactory {
    URLStreamHandler streamHandler = new URLStreamHandler() {
      @Override
      protected URLConnection openConnection(URL url) throws IOException {
        return new StringURLConnection(url);
      }
    };

    @Override
    public URLStreamHandler createURLStreamHandler(String protocol) {
      if ("string".equals(protocol)) {
        return streamHandler;
      }
      return null;
    }
  }
}
