package js7.base.io.process

import js7.base.circeutils.CirceUtils.*
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ProcessSignalTest extends OurTestSuite:
  "JSON" in:
    testJson[ProcessSignal](SIGTERM, json""" "SIGTERM" """)
    testJson[ProcessSignal](SIGKILL, json""" "SIGKILL" """)
