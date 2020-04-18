package cn.xdean.jex.extra.collection.sequence;

import cn.xdean.jex.extra.collection.IntList;

public class IntSequence implements Sequence<Integer> {

  public static IntSequence create(int start, int step) {
    return new IntSequence(start, step);
  }

  private final IntList released;
  private final int start, step;
  private int next;

  private IntSequence(int start, int step) {
    this.released = IntList.create();
    this.start = start;
    this.step = step;
    this.next = start;
  }

  @Override
  public boolean hasNext() {
    return true;
  }

  @Override
  public Integer next() {
    if (released.isEmpty()) {
      int ret = next;
      next += step;
      return ret;
    } else {
      int ret = released.stream().min().getAsInt();
      released.remove(ret);
      return ret;
    }
  }

  @Override
  public boolean release(Integer e) {
    int i = e;
    if (i >= start && i < next && (i - start) % step == 0) {
      return released.add(i);
    }
    return false;
  }
}
