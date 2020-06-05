package js7.data.command

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CancelModeTest extends AnyFreeSpec
{
  "JSON" - {
    "NotStarted" in {
      testJson[CancelMode](CancelMode.NotStarted,
        json"""{
          "TYPE": "NotStarted"
         }""")
    }

    "FreshOrStarted" in {
      testJson[CancelMode](CancelMode.FreshOrStarted,
        json"""{
          "TYPE": "FreshOrStarted"
        } """)
    }
  }
}
