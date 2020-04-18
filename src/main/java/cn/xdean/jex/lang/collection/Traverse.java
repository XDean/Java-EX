package cn.xdean.jex.lang.collection;

import cn.xdean.jex.thrid.rxjava.RxIterator;
import io.reactivex.Flowable;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.Iterator;
import java.util.function.Function;

public class Traverse {
  @FunctionalInterface
  public interface Traversable<T> extends Iterable<T> {
    Flowable<T> traverse(Traverser t);

    default Traverser defaultTraverser() {
      return DefaultTraverser.BREAD_FIRST;
    }

    @Override
    default Iterator<T> iterator() {
      return iterator(defaultTraverser());
    }

    default Iterator<T> iterator(Traverser traverser) {
      return traverse(traverser).to(RxIterator.flowableIterator());
    }

    default Flowable<T> preOrderTraversal() {
      return traverse(DefaultTraverser.PRE_ORDER);
    }

    default Flowable<T> postOrderTraversal() {
      return traverse(DefaultTraverser.POST_ORDER);
    }

    default Flowable<T> breadthFirstTraversal() {
      return traverse(DefaultTraverser.BREAD_FIRST);
    }
  }

  @FunctionalInterface
  public interface Traverser {
    <T> Flowable<T> travese(T root, Function<T, Iterable<T>> getChildren);
  }

  public enum DefaultTraverser implements Traverser {
    PRE_ORDER(Traverse::preOrderTraversal),
    POST_ORDER(Traverse::postOrderTraversal),
    BREAD_FIRST(Traverse::breadthFirstTraversal);

    private final Traverser traverser;

    private DefaultTraverser(Traverser traverser) {
      this.traverser = traverser;
    }

    @Override
    public <T> Flowable<T> travese(T root, Function<T, Iterable<T>> getChildren) {
      return traverser.travese(root, getChildren);
    }
  }

  private static <T> Deque<T> newDeque(T root) {
    Deque<T> deque = new ArrayDeque<>();
    deque.add(root);
    return deque;
  }

  public static <T> Flowable<T> preOrderTraversal(T root, Function<T, Iterable<T>> getChildren) {
    return Flowable.generate(() -> newDeque(Arrays.asList(root).iterator()), (d, e) -> {
      Iterator<T> iterator;
      while (true) {
        if (d.isEmpty()) {
          e.onComplete();
          return;
        }
        iterator = d.peek();
        if (iterator.hasNext()) {
          break;
        } else {
          d.pop();
        }
      }
      T t = iterator.next();
      e.onNext(t);
      Iterator<T> children = getChildren.apply(t).iterator();
      if (children.hasNext()) {
        d.push(children);
      }
    });
  }

  public static <T> Flowable<T> postOrderTraversal(T root, Function<T, Iterable<T>> getChildren) {
    return Flowable.generate(() -> newDeque(Either.<Iterator<T>, T> left(Arrays.asList(root).iterator())), (d, e) -> {
      Either<Iterator<T>, T> item;
      while (true) {
        if (d.isEmpty()) {
          e.onComplete();
          return;
        }
        item = d.peek();
        item.exec(iterator -> {
          if (iterator.hasNext()) {
            T t = iterator.next();
            Iterator<T> children = getChildren.apply(t).iterator();
            d.push(Either.right(t));
            if (children.hasNext()) {
              d.push(Either.left(children));
            }
          } else {
            d.pop();
          }
        }, value -> {
          d.pop();
          e.onNext(value);
        });
        if (item.isRight()) {
          return;
        }
      }
    });
  }

  public static <T> Flowable<T> breadthFirstTraversal(T root, Function<T, Iterable<T>> getChildren) {
    return Flowable.generate(() -> newDeque(Arrays.asList(root).iterator()), (d, e) -> {
      Iterator<T> iterator;
      while (true) {
        if (d.isEmpty()) {
          e.onComplete();
          return;
        }
        iterator = d.peek();
        if (iterator.hasNext()) {
          break;
        } else {
          d.pop();
        }
      }
      T t = iterator.next();
      e.onNext(t);
      Iterator<T> children = getChildren.apply(t).iterator();
      if (children.hasNext()) {
        d.addLast(children);
      }
    });
  }
}
