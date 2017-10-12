package xdean.jex.extra.unit;

import static org.junit.Assert.*;
import static xdean.jex.extra.unit.Length.*;

import org.junit.Test;

public class UnitTest {
  @Test
  public void testLength() throws Exception {
    assertEquals(1, PM.toPM(1));
    assertEquals(1_000, NM.toPM(1));
    assertEquals(1_000_000, UM.toPM(1));
    assertEquals(1e-3, MM.toM(1.0), 0);
    assertEquals(1e-6, UM.toM(1.0), 0);
    assertEquals(1e-9, NM.toM(1.0), 0);
    assertEquals(Long.MAX_VALUE, M.toFM(10000), 0);
    assertEquals(Long.MIN_VALUE, M.toFM(-10000), 0);
  }
}
