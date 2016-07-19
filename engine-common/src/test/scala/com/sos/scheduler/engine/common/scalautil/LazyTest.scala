package com.sos.scheduler.engine.common.scalautil

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class LazyTest extends FreeSpec {

  "lazy" in {
    var i = 0
    val lazyI = Lazy { i += 1; i }
    assert(lazyI() == 1)
    assert(lazyI() == 1)
    assert(i == 1)
  }
}
