package xdean.jex.util.collection;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

import lombok.experimental.UtilityClass;
import rx.Observable;

@UtilityClass
public class TraversalUtil {
  public <T> Observable<T> deepTraversal(T root, Function<T, Iterable<T>> getChildren) {
    return Observable.unsafeCreate(s -> {
      List<T> list = new LinkedList<>();
      List<T> bufferList = new ArrayList<>();
      list.add(root);
      s.onStart();
      while (list.isEmpty() == false) {
        T remove = list.remove(0);
        s.onNext(remove);
        getChildren.apply(remove).forEach(bufferList::add);
        list.addAll(0, bufferList);
        bufferList.clear();
      }
      s.onCompleted();
    });
  }

  public <T> Observable<T> wideTraversal(T root, Function<T, Iterable<T>> getChildren) {
    return Observable.unsafeCreate(s -> {
      List<T> list = new LinkedList<>();
      list.add(root);
      s.onStart();
      while (list.isEmpty() == false) {
        T remove = list.remove(0);
        s.onNext(remove);
        getChildren.apply(remove).forEach(list::add);
      }
      s.onCompleted();
    });
  }
}
