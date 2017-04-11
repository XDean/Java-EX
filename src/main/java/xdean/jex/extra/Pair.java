package xdean.jex.extra;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.ToString;
import lombok.experimental.FieldDefaults;

@Getter
@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@EqualsAndHashCode
@ToString
public class Pair<K, V> {
  K left;
  V right;

  public Pair(K k, V v) {
    this.left = k;
    this.right = v;
  }

  public <L> Pair<L, V> left(L left) {
    return Pair.of(left, right);
  }

  public <R> Pair<K, R> right(R right) {
    return Pair.of(left, right);
  }

  public static <K, V> Pair<K, V> of(K k, V v) {
    return new Pair<>(k, v);
  }

  private static final Pair<?, ?> EMPTY = new Pair<>(null, null);

  @SuppressWarnings("unchecked")
  public static <K, V> Pair<K, V> empty() {
    return (Pair<K, V>) EMPTY;
  }
}
