package xdean.jex.extra.annotation.marker;

import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.*;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.SOURCE)
@Target({ TYPE, METHOD, FIELD })
public @interface NotThreadSafe {

}
