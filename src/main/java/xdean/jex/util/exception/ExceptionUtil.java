package xdean.jex.util.exception;

import java.io.PrintWriter;
import java.io.StringWriter;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ExceptionUtil {  
  public String getStackTraceString(Throwable tr) {
    if (tr == null) {
      return "";
    }
    Throwable t = tr;
    while (t.getCause() != null) {
      t = t.getCause();
    }
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    t.printStackTrace(pw);
    pw.flush();
    return sw.toString();
  }
}
