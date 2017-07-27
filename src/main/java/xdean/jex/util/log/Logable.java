package xdean.jex.util.log;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public interface Logable {
  default Logger logger() {
    return LoggerFactory.getLogger(this.getClass());
  }
}
