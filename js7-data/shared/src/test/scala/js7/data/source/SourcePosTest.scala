package js7.data.source

import js7.base.circeutils.CirceUtils.*
import js7.base.test.Test
import js7.tester.CirceJsonTester.*

/**
  * @author Joacim Zschimmer
  */
final class SourcePosTest extends Test
{
  "JSON" in {
    testJson(SourcePos(1, 2), json"""[1, 2]""")
  }
}
