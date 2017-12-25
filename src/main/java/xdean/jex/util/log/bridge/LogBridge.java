package xdean.jex.util.log.bridge;

import static xdean.jex.util.lang.ExceptionUtil.uncatch;

public class LogBridge {
  public static void install() {
    installSlf4j();
  }

  public static boolean installSlf4j() {
    Class<?> slf4j = uncatch(() -> Class.forName("org.slf4j.bridge.SLF4JBridgeHandler"));
    if (slf4j != null) {
      Slf4jBridge.install();
      return true;
    }
    return false;
  }
}
