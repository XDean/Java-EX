package xdean.jex.util.log;

import org.slf4j.Logger;

import xdean.jex.internal.codecov.CodecovIgnore;

@CodecovIgnore
public interface Logable {
  default Logger log() {
    return LogUtil.log(this);
  }
}
