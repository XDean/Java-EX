package xdean.jex.extra.collection;

import static xdean.jex.util.function.FunctionAdapter.function;
import io.reactivex.Flowable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import lombok.Getter;
import xdean.jex.extra.Wrapper;

import com.google.common.collect.TreeTraverser;

@Getter
public class TreeNode<T> implements Iterable<TreeNode<T>> {
  private static final class Traverser<T> extends TreeTraverser<TreeNode<T>> {
    @Override
    public Iterable<TreeNode<T>> children(TreeNode<T> root) {
      return root.children;
    }
  }

  private TreeNode<T> parent;
  private List<TreeNode<T>> children = new ArrayList<>();
  private T value;

  public TreeNode(T value) {
    this.value = value;
  }

  public TreeNode<T> add(T value) {
    TreeNode<T> added = new TreeNode<>(value);
    added.parent = this;
    children.add(added);
    return added;
  }

  public T remove(T value) {
    return child(value)
        .map(function(n -> n.parent = null))
        .map(children::remove)
        .map(b -> b ? value : null)
        .orElse(null);
  }

  public boolean hasChild(T value) {
    return child(value).isPresent();
  }

  public boolean isLeaf() {
    return children.isEmpty();
  }

  public Optional<TreeNode<T>> child(T value) {
    return children.stream().filter(n -> Objects.equals(value, n.getValue())).findFirst();
  }

  public Optional<TreeNode<T>> deepChild(T value) {
    return Optional.ofNullable(breadthFirst().filter(node -> Objects.equals(node.getValue(), value)).blockingFirst());
  }

  public Optional<TreeNode<T>> commonParent(TreeNode<T> other) {
    List<TreeNode<T>> myParents = parents().toList().blockingGet();
    return other.parents().filter(myParents::contains).map(Optional::of).blockingFirst(Optional.empty());
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

  /*************** Iterator *****************/
  @Override
  public Iterator<TreeNode<T>> iterator() {
    return traverser().breadthFirstTraversal(this).iterator();
  }

  public Flowable<TreeNode<T>> preOrder() {
    return Flowable.fromIterable(() -> traverser().preOrderTraversal(this).iterator());
  }

  public Flowable<TreeNode<T>> postOrder() {
    return Flowable.fromIterable(() -> traverser().postOrderTraversal(this).iterator());
  }

  public Flowable<TreeNode<T>> breadthFirst() {
    return Flowable.fromIterable(() -> traverser().breadthFirstTraversal(this).iterator());
  }

  public TreeTraverser<TreeNode<T>> traverser() {
    return new Traverser<T>();
  }
}