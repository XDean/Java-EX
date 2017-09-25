package xdean.jex.util.collection;

import io.reactivex.Flowable;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.Iterator;
import java.util.function.Function;

import xdean.jex.extra.Either;

public class TraversalUtil {

  private static <T> Deque<T> newDeque(T root) {
    Deque<T> deque = new ArrayDeque<T>();
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
