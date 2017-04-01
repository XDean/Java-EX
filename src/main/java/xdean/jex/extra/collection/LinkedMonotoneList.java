package xdean.jex.extra.collection;

import java.util.AbstractList;
import java.util.Comparator;

import lombok.ToString;
import xdean.jex.extra.collection.LinkedList.Node;

/**
 * A monotone increase list
 * 
 * @author XDean
 *
 * @param <E>
 */
@ToString(includeFieldNames = true, exclude={"compartor"})
public class LinkedMonotoneList<E> extends AbstractList<E> {

  public enum MonoType {
    /** When add an element, put it into the correct position **/
    INSERT,
    /**
     * When add an element, link it to the last, and then delete as less as
     * possible elements to make the list monotone again
     **/
    OVERWRITE;
  }

  private Comparator<E> compartor;
  private LinkedList<E> list = new LinkedList<>();
  private MonoType type;

  public LinkedMonotoneList(Comparator<E> comp, MonoType type) {
    this.compartor = comp;
    this.type = type;
  }

  @Override
  public boolean add(E e) {
    if (type == MonoType.INSERT) {
      Node<E> node = list.last;
      while (compartor.compare(node.item, e) > 0) {
        node = node.prev;
      }
      if (node == list.last) {
        list.linkLast(e);
      } else {
        list.linkBefore(e, node.next);
      }
      return true;
    } else if (type == MonoType.OVERWRITE) {
      while (list.last != null && compartor.compare(list.last.item, e) > 0) {
        list.unlink(list.last);
      }
      list.linkLast(e);
      return true;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public E get(int index) {
    return list.get(index);
  }

  @Override
  public E remove(int index) {
    return list.remove(index);
  }

  @Override
  public boolean remove(Object o) {
    return list.remove(o);
  }

  @Override
  public int size() {
    return list.size;
  }

}
