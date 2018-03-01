package xdean.jex.util.log;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import xdean.codecov.CodecovIgnore;
import xdean.jex.util.log.Log.SubLog;
import xdean.jex.util.reflect.ReflectUtil;

@CodecovIgnore
public class LogUtil {
  static {
    String property = System.getProperty("xdean.jex.log.level");
    property = property == null ? "" : property;
    switch (property.toLowerCase()) {
    case "trace":
      setGlobalLevel(Level.FINEST);
      break;
    case "debug":
      setGlobalLevel(Level.FINER);
      break;
    case "info":
      setGlobalLevel(Level.INFO);
      break;
    case "warning":
      setGlobalLevel(Level.WARNING);
      break;
    case "error":
      setGlobalLevel(Level.SEVERE);
      break;
    default:
      setGlobalLevel(Level.INFO);
    }
  }

  public static void setGlobalLevel(Level newLevel) {
    Logger rootLogger = LogManager.getLogManager().getLogger("");
    rootLogger.setLevel(newLevel);
    for (Handler h : rootLogger.getHandlers()) {
      h.setLevel(newLevel);
    }
  }

  public static Logger logger() {
    return Logger.getLogger(ReflectUtil.getCallerClassName());
  }

  public static Logger logger(Class<?> clz) {
    return Logger.getLogger(clz.getName());
  }

  public static Logger logger(Object o) {
    return logger(o.getClass());
  }

  public static Log log() {
    return Log.of(Logger.getLogger(ReflectUtil.getCallerClassName()));
  }

  public static Log log(Class<?> clz) {
    return Log.of(Logger.getLogger(clz.getName()));
  }

  public static Log log(Object o) {
    return log(o.getClass());
  }

  public static SubLog trace() {
    return Log.of(Logger.getLogger(ReflectUtil.getCallerClassName())).trace();
  }

  public static SubLog trace(Class<?> clz) {
    return Log.of(Logger.getLogger(clz.getName())).trace();
  }

  public static SubLog trace(Object o) {
    return log(o.getClass()).trace();
  }

  public static SubLog debug() {
    return Log.of(Logger.getLogger(ReflectUtil.getCallerClassName())).debug();
  }

  public static SubLog debug(Class<?> clz) {
    return Log.of(Logger.getLogger(clz.getName())).debug();
  }

  public static SubLog debug(Object o) {
    return log(o.getClass()).debug();
  }

  public static SubLog info() {
    return Log.of(Logger.getLogger(ReflectUtil.getCallerClassName())).info();
  }

  public static SubLog info(Class<?> clz) {
    return Log.of(Logger.getLogger(clz.getName())).info();
  }

  public static SubLog info(Object o) {
    return log(o.getClass()).info();
  }

  public static SubLog warning() {
    return Log.of(Logger.getLogger(ReflectUtil.getCallerClassName())).warning();
  }

  public static SubLog warning(Class<?> clz) {
    return Log.of(Logger.getLogger(clz.getName())).warning();
  }

  public static SubLog warning(Object o) {
    return log(o.getClass()).warning();
  }

  public static SubLog error() {
    return Log.of(Logger.getLogger(ReflectUtil.getCallerClassName())).error();
  }

  public static SubLog error(Class<?> clz) {
    return Log.of(Logger.getLogger(clz.getName())).error();
  }

  public static SubLog error(Object o) {
    return log(o.getClass()).error();
  }
}
