package com.sos.jobscheduler.core.event.journal

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
      JournalHeader.Singleton.copy(timestamp = "X"),
      json"""{
        "TYPE": "JobScheduler.Journal",
        "version": "${JournalHeader.Singleton.version}",
        "softwareVersion": "${BuildInfo.version}",
        "buildId": "${BuildInfo.buildId}",
        "timestamp": "X"
      }""")
  }
}
