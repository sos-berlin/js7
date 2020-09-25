package js7.data.event

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester
import org.scalatest.freespec.AnyFreeSpec

final class JournalInfoTest extends AnyFreeSpec
{
  "JSON" in {
    CirceJsonTester.testJson(
      JournalInfo(
        lastEventId = 1L,
        tornEventId = 2L,
        journalFiles = Seq(JournalInfo.JournalFileInfo(
          eventId = 3L,
          size = 4L))),
      json"""{
        "lastEventId": 1,
        "tornEventId": 2,
        "journalFiles": [
          {
            "eventId": 3,
            "size": 4
          }
        ]
      }""")
  }
}
