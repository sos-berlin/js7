package com.sos.jobscheduler.base.time

import cats.syntax.show._
import org.scalatest.FreeSpec
import com.sos.jobscheduler.base.time.Timestamp._

/**
  * @author Joacim Zschimmer
  */
final class TimestampTest extends FreeSpec
{
  "show" in {
    assert(Timestamp.parse("2018-11-21T12:34:56Z").show == "2018-11-21T12:34:56Z")
    assert(Timestamp.parse("2018-11-21T12:34:56.987Z").show == "2018-11-21T12:34:56.987Z")
  }

  "java.util.Date.show" in {
    assert(Timestamp.parse("2018-11-21T12:34:56Z").toJavaUtilDate.show == "2018-11-21T12:34:56Z")
    assert(Timestamp.parse("2018-11-21T12:34:56.987Z").toJavaUtilDate.show == "2018-11-21T12:34:56.987Z")
  }
}
