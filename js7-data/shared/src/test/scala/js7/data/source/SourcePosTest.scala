package js7.data.source

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.*

/**
  * @author Joacim Zschimmer
  */
final class SourcePosTest extends OurTestSuite:
  "JSON" in:
    testJson(SourcePos(1, 2), json"""[1, 2]""")
