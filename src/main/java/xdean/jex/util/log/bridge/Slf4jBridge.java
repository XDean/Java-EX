package xdean.jex.util.log.bridge;

import java.util.logging.LogManager;

import org.slf4j.bridge.SLF4JBridgeHandler;

public class Slf4jBridge {
  public static void install() {
    LogManager.getLogManager().reset();
    SLF4JBridgeHandler.removeHandlersForRootLogger();
    SLF4JBridgeHandler.install();
  }
}
