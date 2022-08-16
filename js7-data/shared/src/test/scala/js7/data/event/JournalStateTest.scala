package js7.data.event

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.*
import js7.base.test.Test
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class JournalStateTest extends Test
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

  "toReleaseEventId" in {
    val a = UserId("A")
    val b = UserId("B")
    val x = UserId("X")

    assert(JournalState.empty.toReleaseEventId(10, Nil) == 10)
    assert(JournalState.empty.toReleaseEventId(10, Seq(a)) == 0)

    val journalState = JournalState(Map(a -> 10, b -> 20))

    assert(journalState.toReleaseEventId(5, Nil) == 5)
    assert(journalState.toReleaseEventId(15, Nil) == 10)
    assert(journalState.toReleaseEventId(25, Nil) == 10)

    assert(journalState.toReleaseEventId(5, Seq(a)) == 5)
    assert(journalState.toReleaseEventId(15, Seq(a)) == 10)
    assert(journalState.toReleaseEventId(25, Seq(a)) == 10)

    assert(journalState.toReleaseEventId(5, Seq(a, b)) == 5)
    assert(journalState.toReleaseEventId(15, Seq(a, b)) == 10)
    assert(journalState.toReleaseEventId(25, Seq(a, b)) == 10)

    assert(journalState.toReleaseEventId(5, Seq(x)) == 0)
    assert(journalState.toReleaseEventId(15, Seq(x)) == 0)
    assert(journalState.toReleaseEventId(25, Seq(x)) == 0)
  }
}
