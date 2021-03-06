package cn.xdean.jex.lang.collection.sequence;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

class IteratorSequence<E> implements Sequence<E> {
  private final Iterator<E> origin;
  private final List<E> released = new ArrayList<>();

  public IteratorSequence(Iterator<E> origin) {
    this.origin = origin;
  }

  @Override
  public boolean hasNext() {
    return !released.isEmpty() || origin.hasNext();
  }

  @Override
  public E next() {
    if (released.isEmpty()) {
      return origin.next();
    } else {
      return released.remove(0);
    }
  }

  @Override
  public boolean release(E e) {
    return released.add(e);
  }
}
