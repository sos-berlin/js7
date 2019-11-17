package com.sos.jobscheduler.core.event.journal.data

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.JournalId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import java.util.UUID
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class JournalHeaderTest extends FreeSpec
{
  "JSON" in {
    testJson[JournalHeader](
      JournalHeader.initial(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))).copy(
        eventId = 777,
        totalEventCount = 999,
        startedAt = Timestamp.parse("2019-05-22T12:00:00.000Z"),
        totalRunningTime = 1.hour,
        timestamp = Timestamp.parse("2019-05-23T22:22:22.222Z")),
      json"""{
        "TYPE": "JobScheduler.Journal",
        "journalId": "ABEiM0RVZneImaq7zN3u_w",
        "eventId": 777,
        "generation": 0,
        "totalEventCount": 999,
        "totalRunningTime": 3600,
        "timestamp": "2019-05-23T22:22:22.222Z",
        "startedAt": "2019-05-22T12:00:00Z",
        "version": "${JournalHeader.Version}",
        "softwareVersion": "${BuildInfo.version}",
        "buildId": "${BuildInfo.buildId}"
      }""")
  }
}
