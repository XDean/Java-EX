package cn.xdean.jex.extra.unit;

import org.junit.Assert;
import org.junit.Test;

public class UnitTest {
  @Test
  public void testLength() throws Exception {
    Assert.assertEquals(1, Length.PM.toPM(1));
    Assert.assertEquals(1_000, Length.NM.toPM(1));
    Assert.assertEquals(1_000_000, Length.UM.toPM(1));
    Assert.assertEquals(1e-3, Length.MM.toM(1.0), 0);
    Assert.assertEquals(1e-6, Length.UM.toM(1.0), 0);
    Assert.assertEquals(1e-9, Length.NM.toM(1.0), 0);
    Assert.assertEquals(Long.MAX_VALUE, Length.M.toFM(10000), 0);
    Assert.assertEquals(Long.MIN_VALUE, Length.M.toFM(-10000), 0);
  }
}
