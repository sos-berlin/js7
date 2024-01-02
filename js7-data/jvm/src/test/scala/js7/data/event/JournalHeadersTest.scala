package js7.data.event

import java.util.UUID
import js7.base.BuildInfo
import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.event.JournalHeadersTest.*
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration.*

final class JournalHeadersTest extends OurTestSuite:
  "JSON" in:
    val journalId = JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))
    testJson[JournalHeader](
      JournalHeaders.initial[TestState](journalId).copy(
        eventId = EventId(777),
        totalEventCount = 999,
        initiallyStartedAt = Timestamp.parse("2019-05-22T12:00:00.000Z"),
        totalRunningTime = 1.hour,
        timestamp = Timestamp.parse("2019-05-23T22:22:22.222Z")),
      json"""{
        "TYPE": "JS7.Journal",
        "typeName": "JournalHeadersTest.TestState",
        "journalId": "ABEiM0RVZneImaq7zN3u_w",
        "eventId": 777,
        "generation": 0,
        "totalEventCount": 999,
        "totalRunningTime": 3600,
        "timestamp": "2019-05-23T22:22:22.222Z",
        "initiallyStartedAt": "2019-05-22T12:00:00Z",
        "version": "${JournalHeader.Version}",
        "js7Version": "${BuildInfo.prettyVersion}",
        "buildId": "${BuildInfo.buildId}"
      }""")

private object JournalHeadersTest:
  final case class TestState(string: String) extends BasicState[TestState]:
    def companion = TestState
  object TestState extends BasicState.Companion[TestState]
