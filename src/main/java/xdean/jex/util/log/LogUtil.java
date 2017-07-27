package xdean.jex.util.log;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import xdean.jex.util.reflect.ReflectUtil;

public class LogUtil {

  public static Logger log(){
    return LoggerFactory.getLogger(ReflectUtil.getCallerClassName());
  }

  public static Logger log(Class<?> clz) {
    return LoggerFactory.getLogger(clz);
  }

  public static Logger log(Object o){
    return log(o.getClass());
  }
}
