package xdean.jex.util.log;

import org.slf4j.Logger;

public interface Logable {
  default Logger log() {
    return LogUtil.log(this);
  }
}
