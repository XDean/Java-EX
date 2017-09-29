package xdean.jex.extra.annotation.marker;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.Documented;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that the element is platform dependent.<br>
 * Provide comments for each platform.
 *
 * @author XDean
 */
@Documented
@Retention(RetentionPolicy.SOURCE)
@Target({ TYPE, METHOD, FIELD, CONSTRUCTOR })
public @interface PlatformDependents {

  /**
   * Common platforms
   */
  String ANDROID = "Android";
  String LINUX = "linux";
  String WINDOWS = "Windows";
  String MAC = "Mac";
  String SOLARIS = "SunOS";
  String IOS = "iOS";

  @Documented
  @Retention(RetentionPolicy.SOURCE)
  @Repeatable(PlatformDependents.class)
  public @interface PlatformDependent {
    /**
     * Dependent platforms
     */
    String[] platform() default "";

    /**
     * Comment about the usage
     *
     * @return
     */
    String comment() default "";

    /**
     * Support these platforms or not.
     */
    boolean support() default true;
  }

  PlatformDependent[] value() default {};
}
