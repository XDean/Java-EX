package xdean.jex.util.log;

import java.util.logging.Logger;

import xdean.codecov.CodecovIgnore;
import xdean.jex.util.log.Log.SubLog;

@CodecovIgnore
public interface Logable {
  default Logger logger() {
    return LogUtil.logger(this);
  }

  default Log log() {
    return LogUtil.log(this);
  }

  default SubLog trace() {
    return log().trace();
  }

  default SubLog debug() {
    return log().debug();
  }

  default SubLog info() {
    return log().info();
  }

  default SubLog warning() {
    return log().warning();
  }

  default SubLog error() {
    return log().error();
  }
}
