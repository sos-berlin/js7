package com.sos.scheduler.engine.data.time

import com.sos.scheduler.engine.data.time.SchedulerDateTime._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SchedulerDateTimeTest extends FreeSpec {
  "test" in {
    formatUtc.apply(Instant.parse("2016-06-29T12:33:44Z")) shouldEqual "2016-06-29T12:33:44Z"
  }
}
