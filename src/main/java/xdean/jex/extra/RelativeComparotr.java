package xdean.jex.extra;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import xdean.jex.extra.collection.TreeNode;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMap.Builder;

public class RelativeComparotr<T> {
  Comparator<T> defaultComparator;
  Map<T, TreeNode<T>> nodeMap;
  TreeNode<T> root;

  List<T> orderList;

  public RelativeComparotr(Comparator<T> defaultComparator) {
    this.defaultComparator = defaultComparator;
    this.orderList = new ArrayList<>();
    this.root = new TreeNode<>(null);
    this.nodeMap = new HashMap<>();
  }

  @SuppressWarnings("unchecked")
  public RelativeComparotr<T> addOrder(T... ts) {
    TreeNode<T> lastNode = null;
    for (T t : ts) {
      TreeNode<T> node = nodeMap.get(t);
      if (node == null) {
        if (lastNode == null) {
          root.add(t);
        } else {
          lastNode.add(t);
        }
      } else if (lastNode != null) {
        if (lastNode.commonParent(node).get() == root) {

        }
      }
    }

    int current = -1;
    for (T t : ts) {
      int index = orderList.indexOf(t);
      if (index == -1) {
        current = orderList.size();
        orderList.add(t);
      } else if (index > current) {
        current = index;
      } else {
        throw new IllegalArgumentException("Conflict order.");
      }
    }
    return this;
  }

  public void sort(T[] array) {
    List<T> toAdd = new ArrayList<>();
    for (T element : array) {

    }
  }

  public Comparator<T> toComparator() {
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
  }
}
