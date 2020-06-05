package js7.data.source

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SourcePosTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(SourcePos(1, 2), json"""[1, 2]""")
  }
}
