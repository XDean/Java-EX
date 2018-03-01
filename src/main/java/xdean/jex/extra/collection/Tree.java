package xdean.jex.extra.collection;

import static xdean.jex.extra.rx2.RxFunctions.rx;
import static xdean.jex.util.function.FunctionAdapter.function;
import static xdean.jex.util.function.Predicates.is;
import static xdean.jex.util.function.Predicates.isEquals;
import static xdean.jex.util.function.Predicates.its;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.google.common.collect.Lists;

import io.reactivex.Flowable;
import xdean.jex.extra.Wrapper;
import xdean.jex.extra.collection.Traverse.Traversable;
import xdean.jex.extra.collection.Traverse.Traverser;

public class Tree<T> implements Traversable<Tree<T>> {
  private Tree<T> parent;
  private List<Tree<T>> children = new ArrayList<>();
  private T value;

  public Tree(T value) {
    this.value = value;
  }

  public Tree<T> getParent() {
    return parent;
  }

  public List<Tree<T>> getChildren() {
    return children;
  }

  public T getValue() {
    return value;
  }

  /**
   * Add the value as child
   *
   * @param value
   * @return the child node
   */
  public Tree<T> add(T value) {
    return add(new Tree<>(value));
  }

  /**
   * Add the node as child
   *
   * @param node
   * @return the child node
   */
  public Tree<T> add(Tree<T> node) {
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
  public boolean remove(Tree<T> node) {
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
  public Optional<Tree<T>> removeFromParent() {
    Tree<T> theParent = this.parent;
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
  public Optional<Tree<T>> getChild(T value) {
    return children.stream().filter(its(n -> n.value, isEquals(value))).findFirst();
  }

  /**
   * Return the given node if it's this node's child
   *
   * @param node
   * @return
   */
  public Optional<Tree<T>> getChild(Tree<T> node) {
    return children.stream().filter(is(node)).findFirst();
  }

  /**
   * Get the first matched child with the given value from all of the sub-tree
   *
   * @param value
   * @return
   */
  public Optional<Tree<T>> deepChild(T value) {
    return breadthFirstTraversal()
        .filter(rx(its(Tree::getValue, isEquals(value))))
        .map(Optional::of)
        .blockingFirst(Optional.empty());
  }

  /**
   * Get the first matched child with the given value from all of the sub-tree
   *
   * @param node
   * @return
   */
  public Optional<Tree<T>> deepChild(Tree<T> node) {
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
  public Optional<Tree<T>> commonParent(Tree<T> other) {
    List<Tree<T>> myParents = parents().startWith(this).toList().blockingGet();
    return other.parents()
        .startWith(other)
        .filter(myParents::contains)
        .map(Optional::of)
        .blockingFirst(Optional.empty());
  }

  public Optional<Flowable<Tree<T>>> pathTo(Tree<T> node) {
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
  private static <T> Flowable<Tree<T>> path(Tree<T> downNode, Tree<T> upNode) {
    return Flowable.generate(() -> Wrapper.of(downNode), (w, e) -> {
      Tree<T> node = w.get();
      if (node == null) {
        e.onComplete();
      } else {
        e.onNext(node);
        if (node == upNode) {
          w.set(null);
        } else {
          Tree<T> parent = node.getParent();
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
  public void swap(Tree<T> node) {
    Tree<T> thisParent = this.parent;
    List<Tree<T>> thisChildren = new ArrayList<>(this.children);

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
  public Flowable<Tree<T>> parents() {
    return Flowable.generate(() -> Wrapper.of(this), (n, e) -> {
      Tree<T> parent = n.get().getParent();
      if (parent != null) {
        e.onNext(parent);
        n.set(parent);
      } else {
        e.onComplete();
      }
    });
  }

  @Override
  public Flowable<Tree<T>> traverse(Traverser traverser) {
    return traverser.travese(this, Tree::getChildren);
  }

  @Override
  public String toString() {
    return "TreeNode [value=" + value + "]";
  }
}