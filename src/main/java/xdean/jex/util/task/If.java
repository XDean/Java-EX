package xdean.jex.util.task;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public class If {

  public static If that(boolean b) {
    return new If(b);
  }

  boolean condition;

  public If todo(Runnable r) {
    if (condition) {
      r.run();
    }
    return this;
  }

  public If otherwise(Runnable r) {
    if (!condition) {
      r.run();
    }
    return this;
  }

  public boolean toBoolean() {
    return condition;
  }
}