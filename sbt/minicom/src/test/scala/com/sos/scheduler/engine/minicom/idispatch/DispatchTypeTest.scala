package com.sos.scheduler.engine.minicom.idispatch

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class DispatchTypeTest extends FreeSpec {

  "DispatchType" in {
    DispatchType.set(0x0F) shouldEqual DispatchType.values.toSet
    DispatchType.set(0x01) shouldEqual Set(DISPATCH_METHOD)
    DispatchType.set(0x02) shouldEqual Set(DISPATCH_PROPERTYGET)
    DispatchType.set(0x03) shouldEqual Set(DISPATCH_METHOD, DISPATCH_PROPERTYGET)
    DispatchType.set(0x04) shouldEqual Set(DISPATCH_PROPERTYPUT)
    DispatchType.set(0x08) shouldEqual Set(DISPATCH_PROPERTYPUTREF)
  }
}
