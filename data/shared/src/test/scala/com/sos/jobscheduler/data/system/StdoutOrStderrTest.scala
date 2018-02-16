package com.sos.jobscheduler.data.system

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StdoutOrStderrTest extends FreeSpec {
  "JSON" in {
    testJson[StdoutOrStderr](Stdout, json""" "stdout" """)
    testJson[StdoutOrStderr](Stderr, json""" "stderr" """)
  }

  "JSON KeyEncoder" in {
    testJson(Map(Stdout → 1, Stderr → 2),
      json"""{
        "stdout": 1,
        "stderr": 2
      }""")
  }
}
