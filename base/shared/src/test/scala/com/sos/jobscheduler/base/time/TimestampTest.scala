package com.sos.jobscheduler.base.time

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TimestampTest extends FreeSpec {

  private val isoString = "2017-12-04T11:22:33.456Z"
  private val millis = 1512386553456L
  private val timestamp = Timestamp.parse(isoString)

  "ofEpochMilli" in {
    assert(timestamp.toEpochMilli == millis)
    assert(Timestamp.ofEpochMilli(millis) == timestamp)
  }

  "ofEpochSecond" in {
    val seconds = 1512386553L
    assert(timestamp.toEpochSecond == seconds)
    assert(Timestamp.ofEpochSecond(seconds) == Timestamp.parse("2017-12-04T11:22:33Z"))
  }

  "toIsoString" in {
    assert(timestamp.toIsoString == isoString)
  }
}
