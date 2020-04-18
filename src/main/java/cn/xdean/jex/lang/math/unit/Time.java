package cn.xdean.jex.lang.math.unit;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.TimeUnit;

import static java.lang.annotation.ElementType.*;

public class Time {
  @Retention(RetentionPolicy.SOURCE)
  @Target({ METHOD, FIELD, PARAMETER, TYPE, TYPE_USE, LOCAL_VARIABLE })
  public @interface Unit {
    TimeUnit value();
  }
}
