package com.sos.jobscheduler.shared.event.journal

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
        "version": "0.1",
        "softwareVersion": "${BuildInfo.version}"
        }""")
  }
}
