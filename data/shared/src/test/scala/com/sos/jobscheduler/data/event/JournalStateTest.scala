package js7.data.event

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JournalStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      JournalState(Map(UserId("A-USER") -> EventId(1000))),
      json"""{
      "userIdToReleasedEventId": {
        "A-USER": 1000
      }
    }""")
  }
}
