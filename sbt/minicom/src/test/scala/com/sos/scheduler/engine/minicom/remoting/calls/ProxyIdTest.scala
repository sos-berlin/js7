package com.sos.scheduler.engine.minicom.remoting.calls

import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
final class ProxyIdTest extends FreeSpec {

  "newGenerator" in {
    val generate = ProxyId.newGenerator().next _
    for (i ← 0x40000001 to 0x40000001 + 10000) {
      generate().index shouldEqual i
    }
  }

  "toString" in {
    assert(ProxyId(0xf0abc1234L).toString == "ProxyId(0000000F.0ABC1234)")
  }
}
