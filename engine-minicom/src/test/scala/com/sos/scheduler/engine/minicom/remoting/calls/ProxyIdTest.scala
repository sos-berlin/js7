package com.sos.scheduler.engine.minicom.remoting.calls

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProxyIdTest extends FreeSpec {

  "newGenerator" in {
    val generate = ProxyId.newGenerator().next _
    for (i ← 1 to 10000) {
      generate().index shouldEqual i
    }
  }
}
