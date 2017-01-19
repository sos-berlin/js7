package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.TimeoutWithSteps._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class TimeoutWithStepsTest extends FreeSpec {

  "instantIterator" in {
    millisInstantIterator(100, 7, 7).toList shouldEqual List(100, 100 + 7)
    millisInstantIterator(100, 7, 3).toList shouldEqual List(100, 100 + 3, 100 + 6, 100 + 7)
    millisInstantIterator(100, 3, 7).toList shouldEqual List(100, 100 + 3)
  }
}
