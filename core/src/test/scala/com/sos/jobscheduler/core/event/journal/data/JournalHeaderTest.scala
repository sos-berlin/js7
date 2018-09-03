package com.sos.jobscheduler.core.event.journal.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalHeaderTest extends FreeSpec {

  "NamedJsonFormat" in {
    testJson[JournalHeader](
      JournalHeader(eventId = 777, totalEventCount = 999).copy(timestamp = "X"),
      json"""{
        "TYPE": "JobScheduler.Journal",
        "version": "${JournalHeader.Version}",
        "softwareVersion": "${BuildInfo.version}",
        "buildId": "${BuildInfo.buildId}",
        "eventId": 777,
        "totalEventCount": 999,
        "timestamp": "X"
      }""")
  }
}
