package com.sos.jobscheduler.master.gui.data

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TimestampTest extends FreeSpec {

  "epochMilli" in {
    val iso = "2117-11-09T12:33:44.123Z"
    val epochMilli = 4665904424123L
    val ts = Timestamp(iso)
    assert(ts.toEpochMilli == epochMilli)
    assert(ts == Timestamp.fromEpochMilli(epochMilli))
    assert(ts.toString == iso)
  }

  "epochMilli 1900" in {
    val iso = "1900-00-00T00:00:00.123Z"
    val epochMilli = 2208988800000L + 123
    val ts = Timestamp.fromEpochMilli(epochMilli)
    assert(ts.toEpochMilli == epochMilli)
    assert(ts == Timestamp.fromEpochMilli(epochMilli))
    assert(ts.toString == iso)
  }

  "Start of epoch, and zero milliseconds" in {
    val iso = "1970-01-01T00:00:00Z"
    val ts = Timestamp(iso)
    assert(ts.toEpochMilli == 0)
    assert(ts == Timestamp.fromEpochMilli(0))
    assert(ts.toString == iso)
  }
}
