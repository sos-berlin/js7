package js7.data.system

import js7.base.circeutils.CirceUtils.*
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class StdoutOrStderrTest extends OurTestSuite:
  "JSON" in:
    testJson[StdoutOrStderr](Stdout, json""" "stdout" """)
    testJson[StdoutOrStderr](Stderr, json""" "stderr" """)

  "JSON KeyEncoder" in:
    testJson(Map(Stdout -> 1, Stderr -> 2),
      json"""{
        "stdout": 1,
        "stderr": 2
      }""")
