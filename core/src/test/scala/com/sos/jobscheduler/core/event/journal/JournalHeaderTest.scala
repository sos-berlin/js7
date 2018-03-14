package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalHeaderTest extends FreeSpec {

  "NamedJsonFormat" in {
    testJson[JournalHeader](JournalMeta.Header,
      s"""{
        "TYPE": "JobScheduler.Journal",
        "version": "${JournalMeta.Header.version}",
        "softwareVersion": "${BuildInfo.version}",
        "buildId": "${BuildInfo.buildId}"
      }""")
  }
}
