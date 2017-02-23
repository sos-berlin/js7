package com.sos.scheduler.engine.base.utils

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PerKeyLimiterTest extends FreeSpec {

  "PerKeyLimiter" in {
    def perKeyLimiter(limit: Int) = new PerKeyLimiter(limit, (v: (Int, Int)) ⇒ v._1)

    intercept[IllegalArgumentException] { perKeyLimiter(-1) }

    assert((List[(Int, Int)]() filter perKeyLimiter(1)) == Nil)
    assert((List(1 → 10) filter perKeyLimiter(0)) == Nil)
    assert((List(1 → 10) filter perKeyLimiter(1)) == List(1 → 10))
    assert((List(1 → 10, 1 → 11) filter perKeyLimiter(1)) == List(1 → 10))
    assert((List(1 → 10, 1 → 11, 1 → 12, 1 → 13) filter perKeyLimiter(1)) == List(1 → 10))
    assert((List(1 → 10, 1 → 11, 1 → 12, 1 → 13, 2 → 20) filter perKeyLimiter(1)) ==
            List(1 → 10,                         2 → 20))
    assert((List(1 → 10, 1 → 11, 1 → 12, 1 → 13, 1 → 14, 2 → 20, 2 → 21, 2 → 22) filter new PerKeyLimiter[Int, (Int, Int)](2, _._1)) ==
            List(1 → 10, 1 → 11,                         2 → 20, 2 → 21))
    assert((List(1 → 10, 1 → 11, 1 → 12, 1 → 13, 1 → 14, 2 → 20, 2 → 21, 2 → 22, 2 → 23, 2 → 24) filter new PerKeyLimiter[Int, (Int, Int)](3, _._1)) ==
            List(1 → 10, 1 → 11, 1 → 12,                 2 → 20, 2 → 21, 2 → 22))
  }
}
