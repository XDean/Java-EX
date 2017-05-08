package xdean.jex.config;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@UtilityClass
public class Config {
  private final Properties CONFIG = new Properties();
  private Path configFile;

  public void locate(Path configPath, Path defaultConfig) {
    try {
      if (Files.notExists(configPath)) {
        if (Files.exists(defaultConfig)) {
          Files.copy(defaultConfig, configPath);
        } else {
          Files.createFile(configPath);
        }
      }
      CONFIG.load(Files.newBufferedReader(configPath));
    } catch (IOException e) {
      log.error("IOException", e);
    }
    log.debug("Load last config: " + CONFIG.toString());
    configFile = configPath;
  }

  public Optional<String> getProperty(String key) {
    return Optional.ofNullable(CONFIG.getProperty(key));
  }

  public Optional<String> getProperty(Object key) {
    return getProperty(key.toString());
  }

  public String getProperty(Object key, String defaultValue) {
    return getProperty(key.toString(), defaultValue);
  }

  public String getProperty(String key, String defaultValue) {
    return getProperty(key).orElse(defaultValue);
  }

  public void setProperty(Object key, String value) {
    setProperty(key.toString(), value);
  }

  public void setProperty(String key, String value) {
    CONFIG.setProperty(key, value);
    save();
  }

  public void setIfAbsent(Object key, String value) {
    setIfAbsent(key.toString(), value);
  }

  public void setIfAbsent(String key, String value) {
    if (getProperty(key).isPresent() == false) {
      setProperty(key, value);
    }
  }

  private synchronized void save() {
    if (configFile == null) {
      return;
    }
    try {
      CONFIG.store(Files.newOutputStream(configFile), "");
    } catch (IOException e) {
      log.error("", e);
    }
  }
}
