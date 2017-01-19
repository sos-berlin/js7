package com.sos.scheduler.engine.common.utils

import org.scalatest.FunSuite
import org.scalatest.Matchers._

final class RandomsTest extends FunSuite {
  test("randomInt") {
    val range = 1 to 10
    for (i <- 1 to 1000)
      range should contain (Randoms.randomInt(range))
  }

  test("randomInts") {
    val range = 1 to 10
    for (i <- 1 to 1000) {
      val o = Randoms.randomStartInts(range).toSeq
      o should have size range.size
      o.toSet should have size range.size
    }
  }
}
