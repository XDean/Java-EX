package xdean.jex.extra;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import xdean.jex.extra.collection.TreeNode;

import com.google.common.annotations.Beta;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMap.Builder;

/**
 *
 *
 * @author XDean
 *
 * @param <T>
 */
@Beta
public class RelativeComparator<T> {
  public static <T extends Comparable<T>> RelativeComparator<T> create() {
    return new RelativeComparator<T>(Comparator.naturalOrder());
  }

  public static <T> RelativeComparator<T> create(Comparator<T> defaultComparator) {
    return new RelativeComparator<>(defaultComparator);
  }

  @SafeVarargs
  public static <T extends Comparable<T>> Comparator<T> of(T... ts) {
    return new RelativeComparator<T>(Comparator.naturalOrder()).addOrder(ts).toComparator();
  }

  @SafeVarargs
  public static <T> Comparator<T> of(Comparator<T> defaultComparator, T... ts) {
    return new RelativeComparator<T>(defaultComparator).addOrder(ts).toComparator();
  }

  Comparator<T> defaultComparator;
  TreeNode<T> root;

  public RelativeComparator(Comparator<T> defaultComparator) {
    this.defaultComparator = defaultComparator;
    this.root = new TreeNode<>(null);
  }

  @SuppressWarnings("unchecked")
  public RelativeComparator<T> addOrder(T... ts) {
    for (int i = 0; i < ts.length - 1; i++) {
      addOrder(ts[i], ts[i + 1]);
    }
    return this;
  }

  public RelativeComparator<T> addOrder(T small, T big) {
    Optional<TreeNode<T>> oSmallNode = root.deepChild(small);
    TreeNode<T> smallNode;
    if (oSmallNode.isPresent()) {
      smallNode = oSmallNode.get();
    } else {
      smallNode = root.add(small);
    }

    Optional<TreeNode<T>> oBigNode = root.deepChild(big);
    if (oBigNode.isPresent()) {
      TreeNode<T> bigNode = oBigNode.get();
      TreeNode<T> commonParent = smallNode.commonParent(bigNode).get();
      if (commonParent == smallNode) {
        return this;
      } else if (commonParent == bigNode) {
        throw new IllegalArgumentException(String.format(
            "%s is already bigger than %s, can't setter it as the smaller.", small, big));
      } else if (commonParent == bigNode.getParent()) {
        smallNode.add(bigNode);
      } else if (commonParent == smallNode.getParent()) {
        throw new UnsupportedOperationException();
      } else {
        throw new UnsupportedOperationException();
      }
    } else {
      smallNode.add(big);
    }
    return this;
  }

  public Comparator<T> toComparator() {
    return root.breadthFirstTraversal()
        .skip(1)
        .map(TreeNode::getValue)
        .toList()
        .<Comparator<T>> map(orderList -> {
          List<T> target = new ArrayList<>(orderList);
          target.sort(defaultComparator);
          if (orderList.equals(target)) {
            return defaultComparator;
          } else {
            Builder<T, T> builder = ImmutableMap.builder();
            for (int i = 0; i < orderList.size(); i++) {
              builder.put(orderList.get(i), target.get(i));
            }
            ImmutableMap<T, T> map = builder.build();
            return (a, b) -> defaultComparator.compare(map.getOrDefault(a, a), map.getOrDefault(b, b));
          }
        })
        .blockingGet();
  }
}
