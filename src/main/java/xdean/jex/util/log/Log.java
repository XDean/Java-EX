package xdean.jex.util.log;

import java.util.ResourceBundle;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Log {
  public static Log of(Logger logger) {
    return new Log(logger);
  }

  Logger logger;

  public Log(Logger logger) {
    this.logger = logger;
  }

  public SubLog trace() {
    return new SubLog(Level.FINEST);
  }

  public SubLog debug() {
    return new SubLog(Level.FINE);
  }

  public SubLog info() {
    return new SubLog(Level.INFO);
  }

  public SubLog warning() {
    return new SubLog(Level.WARNING);
  }

  public SubLog error() {
    return new SubLog(Level.SEVERE);
  }

  public class SubLog {
    Level level;

    public SubLog(Level level) {
      this.level = level;
    }

    public void log(String msg) {
      logger.log(level, msg);
    }

    public void log(Supplier<String> msgSupplier) {
      logger.log(level, msgSupplier);
    }

    public void log(String msg, Object param1) {
      logger.log(level, msg, param1);
    }

    public void log(String msg, Object... params) {
      logger.log(level, msg, params);
    }

    public void log(String msg, Throwable thrown) {
      logger.log(level, msg, thrown);
    }

    public void log(Throwable thrown, Supplier<String> msgSupplier) {
      logger.log(level, thrown, msgSupplier);
    }

    public void logp(String sourceClass, String sourceMethod, String msg) {
      logger.logp(level, sourceClass, sourceMethod, msg);
    }

    public void logp(String sourceClass, String sourceMethod, Supplier<String> msgSupplier) {
      logger.logp(level, sourceClass, sourceMethod, msgSupplier);
    }

    public void logp(String sourceClass, String sourceMethod, String msg, Object param1) {
      logger.logp(level, sourceClass, sourceMethod, msg, param1);
    }

    public void logp(String sourceClass, String sourceMethod, String msg, Object[] params) {
      logger.logp(level, sourceClass, sourceMethod, msg, params);
    }

    public void logp(String sourceClass, String sourceMethod, String msg, Throwable thrown) {
      logger.logp(level, sourceClass, sourceMethod, msg, thrown);
    }

    public void logp(String sourceClass, String sourceMethod, Throwable thrown, Supplier<String> msgSupplier) {
      logger.logp(level, sourceClass, sourceMethod, thrown, msgSupplier);
    }

    public void logrb(String sourceClass, String sourceMethod, ResourceBundle bundle, String msg, Object... params) {
      logger.logrb(level, sourceClass, sourceMethod, bundle, msg, params);
    }

    public void logrb(String sourceClass, String sourceMethod, ResourceBundle bundle, String msg, Throwable thrown) {
      logger.logrb(level, sourceClass, sourceMethod, bundle, msg, thrown);
    }
  }
}
