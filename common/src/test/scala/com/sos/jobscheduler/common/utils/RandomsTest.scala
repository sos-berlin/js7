package com.sos.jobscheduler.common.utils

import org.scalatest.matchers
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite

final class RandomsTest extends AnyFunSuite {
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
