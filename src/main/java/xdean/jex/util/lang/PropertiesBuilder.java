package xdean.jex.util.lang;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class PropertiesBuilder {

  public static PropertiesBuilder create() {
    return new PropertiesBuilder();
  }

  Map<String, String> values = new HashMap<>();
  Properties defaults;

  public PropertiesBuilder put(String key, String value) {
    values.put(key, value);
    return this;
  }

  public PropertiesBuilder defaults(Properties defaults) {
    this.defaults = defaults;
    return this;
  }

  public Properties build() {
    Properties p = defaults == null ? new Properties() : new Properties(defaults);
    values.forEach((k, v) -> p.setProperty(k, v));
    return p;
  }
}
