package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.time.TimeoutWithSteps._
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

final class TimeoutWithStepsTest extends AnyFreeSpec {

  "instantIterator" in {
    assert(deadlineIterator(Deadline(100.ms), 7.ms, 7.ms).toList == Deadline(100.ms) :: Deadline(107.ms) :: Nil)
    assert(deadlineIterator(Deadline(100.ms), 7.ms, 3.ms).toList == Deadline(100.ms) :: Deadline(103.ms) :: Deadline(106.ms) :: Deadline(107.ms) :: Nil)
    assert(deadlineIterator(Deadline(100.ms), 3.ms, 7.ms).toList == Deadline(100.ms) :: Deadline(103.ms) :: Nil)
  }
}
