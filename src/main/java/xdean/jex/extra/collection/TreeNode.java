package xdean.jex.extra.collection;

import static xdean.jex.extra.rx2.RxFunctions.rx;
import static xdean.jex.util.function.FunctionAdapter.function;
import static xdean.jex.util.function.Predicates.*;
import io.reactivex.Flowable;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import lombok.Getter;
import xdean.jex.extra.Wrapper;
import xdean.jex.extra.collection.Traverse.Traversable;
import xdean.jex.extra.collection.Traverse.Traverser;

import com.google.common.collect.Lists;

@Getter
public class TreeNode<T> implements Traversable<TreeNode<T>> {
  private TreeNode<T> parent;
  private List<TreeNode<T>> children = new ArrayList<>();
  private T value;

  public TreeNode(T value) {
    this.value = value;
  }

  /**
   * Add the value as child
   *
   * @param value
   * @return the child node
   */
  public TreeNode<T> add(T value) {
    return add(new TreeNode<>(value));
  }

  /**
   * Add the node as child
   *
   * @param node
   * @return the child node
   */
  public TreeNode<T> add(TreeNode<T> node) {
    node.removeFromParent();
    node.parent = this;
    children.add(node);
    return node;
  }

  /**
   * Remove the first matched node with given value from children
   *
   * @param value
   * @return has removed
   */
  public boolean remove(T value) {
    return getChild(value)
        .map(this::remove)
        .orElse(false);
  }

  /**
   * Remove the node from children
   *
   * @param node
   * @return has removed
   */
  public boolean remove(TreeNode<T> node) {
    return getChild(node)
        .map(function(n -> n.parent = null))
        .map(children::remove)
        .orElse(false);
  }

  /**
   * Remove this node from its parent
   *
   * @return the old parent
   */
  public Optional<TreeNode<T>> removeFromParent() {
    TreeNode<T> theParent = this.parent;
    if (parent != null) {
      parent.remove(this);
    }
    return Optional.ofNullable(theParent);
  }

  /**
   * Returns true if there is a child's value is the given value
   *
   * @param value
   * @return
   */
  public boolean hasChild(T value) {
    return getChild(value).isPresent();
  }

  /**
   * Returns true if the node has no children.
   *
   * @return
   */
  public boolean isLeaf() {
    return children.isEmpty();
  }

  /**
   * Get the first matched child with the given value from children
   *
   * @param value
   * @return
   */
  public Optional<TreeNode<T>> getChild(T value) {
    return children.stream().filter(its(n -> n.value, isEquals(value))).findFirst();
  }

  /**
   * Return the given node if it's this node's child
   *
   * @param node
   * @return
   */
  public Optional<TreeNode<T>> getChild(TreeNode<T> node) {
    return children.stream().filter(is(node)).findFirst();
  }

  /**
   * Get the first matched child with the given value from all of the sub-tree
   *
   * @param value
   * @return
   */
  public Optional<TreeNode<T>> deepChild(T value) {
    return breadthFirstTraversal()
        .filter(rx(its(TreeNode::getValue, isEquals(value))))
        .map(Optional::of)
        .blockingFirst(Optional.empty());
  }

  /**
   * Get the first matched child with the given value from all of the sub-tree
   *
   * @param node
   * @return
   */
  public Optional<TreeNode<T>> deepChild(TreeNode<T> node) {
    return breadthFirstTraversal()
        .filter(rx(is(node)))
        .map(Optional::of)
        .blockingFirst(Optional.empty());
  }

  /**
   * Get the first common parent of this node and the given node
   *
   * @param other
   * @return the common parent
   */
  public Optional<TreeNode<T>> commonParent(TreeNode<T> other) {
    List<TreeNode<T>> myParents = parents().startWith(this).toList().blockingGet();
    return other.parents()
        .startWith(other)
        .filter(myParents::contains)
        .map(Optional::of)
        .blockingFirst(Optional.empty());
  }

  public Optional<Flowable<TreeNode<T>>> pathTo(TreeNode<T> node) {
    if (deepChild(node).isPresent()) {
      return Optional.of(
          path(node, this)
              .toList()
              .flattenAsFlowable(Lists::reverse));
    } else if (node.deepChild(this).isPresent()) {
      return Optional.of(path(this, node));
    } else {
      return Optional.empty();
    }
  }

  /**
   * @param downNode
   * @param upNode
   * @return from downNode to upNode's path
   */
  private static <T> Flowable<TreeNode<T>> path(TreeNode<T> downNode, TreeNode<T> upNode) {
    return Flowable.generate(() -> Wrapper.of(downNode), (w, e) -> {
      TreeNode<T> node = w.get();
      if (node == null) {
        e.onComplete();
      } else {
        e.onNext(node);
        if (node == upNode) {
          w.set(null);
        } else {
          TreeNode<T> parent = node.getParent();
          w.set(parent);
        }
      }
    });
  }

  /**
   * Swap with the given node. Even they are in different trees.
   *
   * @param node
   */
  public void swap(TreeNode<T> node) {
    TreeNode<T> thisParent = this.parent;
    List<TreeNode<T>> thisChildren = new ArrayList<>(this.children);

    this.removeFromParent();
    if (node.parent != null) {
      node.parent.add(this);
    }
    node.children.forEach(this::add);

    node.removeFromParent();
    if (thisParent != null) {
      thisParent.add(node);
    }
    thisChildren.forEach(node::add);
  }

  public void rotateAsParent() {
    removeFromParent().ifPresent(this::add);
  }

  /**
   * Get all of the node's parent
   *
   * @return
   */
  public Flowable<TreeNode<T>> parents() {
    return Flowable.generate(() -> Wrapper.of(this), (n, e) -> {
      TreeNode<T> parent = n.get().getParent();
      if (parent != null) {
        e.onNext(parent);
        n.set(parent);
      } else {
        e.onComplete();
      }
    });
  }

  @Override
  public Flowable<TreeNode<T>> traverse(Traverser traverser) {
    return traverser.travese(this, TreeNode::getChildren);
  }

  @Override
  public String toString() {
    return "TreeNode [value=" + value + "]";
  }
}