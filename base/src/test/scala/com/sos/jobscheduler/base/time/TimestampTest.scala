package com.sos.jobscheduler.base.time

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TimestampTest extends FreeSpec {

  "ofEpochMilli" in {
    assert(Timestamp.ofEpochMilli(123456).toEpochMilli == 123456)
  }

  "ofEpochSecond" in {
    assert(Timestamp.ofEpochSecond(123).toEpochMilli == 123000)
  }
}
