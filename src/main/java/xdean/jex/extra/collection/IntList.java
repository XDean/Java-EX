package xdean.jex.extra.collection;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.RandomAccess;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class IntList implements RandomAccess, Cloneable, java.io.Serializable {

  private static final int DEFAULT_CAPACITY = 10;

  int[] elementData;
  int size;

  public IntList() {
    this(DEFAULT_CAPACITY);
  }

  public IntList(int initialCapacity) {
    elementData = new int[initialCapacity];
  }

  public IntList(int[] array) {
    elementData = array;
    size = array.length;
  }

  public int size() {
    return size;
  }

  public boolean isEmpty() {
    return size == 0;
  }

  public boolean contains(int i) {
    return IntStream.of(elementData).limit(size).anyMatch(a -> a == i);
  }

  public boolean containsAll(int[] is) {
    for (int i : is) {
      if (!contains(i)) {
        return false;
      }
    }
    return true;
  }

  public Iterator<Integer> iterator() {
    return IntStream.of(elementData).limit(size).iterator();
  }

  public int[] toArray() {
    return Arrays.copyOf(elementData, size);
  }

  public boolean add(int i) {
    ensureCapacity(size + 1);
    elementData[size++] = i;
    return true;
  }

  public void add(int index, int element) {
    rangeCheckForAdd(index);
    ensureCapacity(size + 1);
    System.arraycopy(elementData, index, elementData, index + 1, size - index);
    elementData[index] = element;
    size++;
  }

  public boolean addAll(int[] is) {
    int numNew = is.length;
    ensureCapacity(size + numNew);
    System.arraycopy(is, 0, elementData, size, numNew);
    size += numNew;
    return numNew != 0;
  }

  public boolean addAll(int index, int[] is) {
    rangeCheckForAdd(index);
    boolean modified = false;
    for (int i : is) {
      add(index++, i);
      modified = true;
    }
    return modified;
  }

  public boolean remove(int i) {
    for (int index = 0; index < size; index++) {
      if (i == elementData[index]) {
        fastRemove(index);
        return true;
      }
    }
    return false;
  }

  public int removeIndex(int index) {
    rangeCheck(index);

    int oldValue = elementData[index];
    int numMoved = size - index - 1;
    if (numMoved > 0) {
      System.arraycopy(elementData, index + 1, elementData, index, numMoved);
    }
    return oldValue;
  }

  public boolean removeAll(int[] is) {
    return batchRemove(is, false);
  }

  public boolean retainAll(int[] is) {
    return batchRemove(is, true);
  }

  public void clear() {
    size = 0;
  }

  public int get(int index) {
    rangeCheck(index);
    return elementData[index];
  }

  public int set(int index, int element) {
    rangeCheck(index);
    int oldValue = elementData[index];
    elementData[index] = element;
    return oldValue;
  }

  public int indexOf(int i) {
    for (int idx = 0; idx < size; idx++) {
      if (i == elementData[idx]) {
        return idx;
      }
    }
    return -1;
  }

  public int lastIndexOf(int i) {
    for (int idx = size - 1; idx >= 0; idx--) {
      if (i == elementData[idx]) {
        return idx;
      }
    }
    return -1;
  }

  public IntStream stream() {
    return IntStream.of(elementData).limit(size);
  }

  public List<Integer> boxed() {
    return stream().boxed().collect(Collectors.toList());
  }

  /************************ private methods same as ArrayList ****************************/
  private void fastRemove(int index) {
    int numMoved = size - index - 1;
    if (numMoved > 0) {
      System.arraycopy(elementData, index + 1, elementData, index, numMoved);
    }
  }

  private boolean batchRemove(int[] is, boolean complement) {
    return batchRemove(new IntList(is), complement);
  }

  private boolean batchRemove(IntList il, boolean complement) {
    final int[] elementData = this.elementData;
    int r = 0, w = 0;
    try {
      for (; r < size; r++) {
        if (il.contains(elementData[r]) == complement) {
          elementData[w++] = elementData[r];
        }
      }
    } finally {
      if (r != size) {
        System.arraycopy(elementData, r, elementData, w, size - r);
        w += size - r;
      }
      if (w != size) {
        size = w;
      }
    }
    return true;
  }

  private void ensureCapacity(int minCapacity) {
    if (minCapacity >= elementData.length) {
      int newCapacity = Math.max(minCapacity, elementData.length + (elementData.length >> 1));
      elementData = Arrays.copyOf(elementData, newCapacity);
    }
  }

  private void rangeCheck(int index) {
    if (index >= size) {
      throw new IndexOutOfBoundsException(outOfBoundsMsg(index));
    }
  }

  private void rangeCheckForAdd(int index) {
    if (index > size || index < 0) {
      throw new IndexOutOfBoundsException(outOfBoundsMsg(index));
    }
  }

  private String outOfBoundsMsg(int index) {
    return "Index: " + index + ", Size: " + size;
  }
}