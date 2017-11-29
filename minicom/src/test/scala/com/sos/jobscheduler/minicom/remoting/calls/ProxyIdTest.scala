package com.sos.jobscheduler.minicom.remoting.calls

import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class ProxyIdTest extends FreeSpec {

  "newGenerator" in {
    val generator = ProxyId.newGenerator()
    for (i ← 0x40000001 to 0x40000001 + 10000) {
      generator.next().index shouldEqual i
    }
  }

  "toString" in {
    assert(ProxyId(0xf0abc1234L).toString == "ProxyId(0000000F.0ABC1234)")
  }
}
