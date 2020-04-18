package cn.xdean.jex.lang.collection.sequence;

import java.util.Iterator;

public interface Sequence<E> extends Iterator<E> {
  @Override
  boolean hasNext();

  @Override
  E next();

  boolean release(E e);
}
