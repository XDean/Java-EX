package xdean.jex.extra.annotation.marker;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.Documented;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that the element is component dependent.<br>
 * Provide comments for each component.
 *
 * @author XDean
 */
@Documented
@Retention(RetentionPolicy.SOURCE)
@Target({ TYPE, METHOD, FIELD, CONSTRUCTOR })
public @interface ComponentDependents {
  @Documented
  @Retention(RetentionPolicy.SOURCE)
  @Repeatable(ComponentDependents.class)
  public @interface ComponentDependent {
    /**
     * Dependent components
     */
    String[] component() default "";

    /**
     * Comment about the usage
     *
     * @return
     */
    String comment() default "";

    /**
     * Support these components or not.
     */
    boolean support() default true;
  }

  ComponentDependent[] value() default {};
}
