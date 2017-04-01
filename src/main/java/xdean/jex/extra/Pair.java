package xdean.jex.extra;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.FieldDefaults;

@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@EqualsAndHashCode
@ToString
public class Pair<K, V> {
  K left;
  V right;

  public Pair(K k, V v) {
    this.left = k;
    this.right = v;
  }

  public static <K, V> Pair<K, V> of(K k, V v) {
    return new Pair<>(k, v);
  }
}
