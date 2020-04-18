package cn.xdean.jex.lang.collection;

import java.util.AbstractList;
import java.util.Arrays;

/**
 * Simple fixed length list with just {@link #add(Object)} and {@link #get(int)}
 *
 * @author XDean
 *
 * @param <T>
 */
public class FixedLengthList<T> extends AbstractList<T> {

  protected final int length;
  protected final T[] array;
  protected int pos;
  protected int count;

  @SuppressWarnings("unchecked")
  public FixedLengthList(int length) {
    this.length = length;
    this.array = (T[]) new Object[length];
  }

  @Override
  public int size() {
    return count >= length ? length : pos;
  }

  @Override
  public T get(int index) {
    return count >= length ? array[(pos + index) % length] : array[index];
  }

  @Override
  public boolean add(T t) {
    array[pos] = t;
    pos = (pos + 1) % length;
    count++;
    return true;
  }

  @Override
  public Object[] toArray() {
    Object[] buffer;
    if (array[pos] == null) {
      buffer = Arrays.copyOf(array, pos);
    } else {
      buffer = new Object[length];
      System.arraycopy(array, pos, buffer, 0, length - pos);
      System.arraycopy(array, 0, buffer, length - pos, pos);
    }
    return buffer;
  }
}