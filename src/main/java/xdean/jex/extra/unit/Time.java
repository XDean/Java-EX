package xdean.jex.extra.unit;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.concurrent.TimeUnit;

public class Time {
  @Retention(RetentionPolicy.SOURCE)
  @Target({ METHOD, FIELD, PARAMETER, TYPE, TYPE_USE, LOCAL_VARIABLE })
  public @interface Unit {
    TimeUnit value();
  }
}
