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

@Getter
public class TreeNode<T> implements Iterable<TreeNode<T>>, Traversable<TreeNode<T>> {
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
    TreeNode<T> added = new TreeNode<>(value);
    added.parent = this;
    children.add(added);
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
   */
  public void removeFromParent() {
    parent.remove(this);
    parent = null;
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
    return Optional.ofNullable(breadthFirstTraversal()
        .filter(rx(its(TreeNode::getValue, isEquals(value))))
        .blockingFirst());
  }

  public Optional<TreeNode<T>> commonParent(TreeNode<T> other) {
    List<TreeNode<T>> myParents = parents().startWith(this).toList().blockingGet();
    return other.parents()
        .startWith(this)
        .filter(myParents::contains)
        .map(Optional::of)
        .blockingFirst(Optional.empty());
  }

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
}