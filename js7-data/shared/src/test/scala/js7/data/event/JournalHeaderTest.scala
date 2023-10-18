package js7.data.event

import java.nio.file.Paths
import java.util.UUID
import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.event.JournalHeader.{JournalIdMismatchProblem, JournalTypeMismatchProblem}
import js7.data.event.JournalHeaderTest.*
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class JournalHeaderTest extends OurTestSuite:
  "JSON" in:
    testJson[JournalHeader](
      JournalHeader(
        typeName = Some("TestState"),
        JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")),
        eventId = EventId(777),
        totalEventCount = 999,
        initiallyStartedAt = Timestamp.parse("2019-05-22T12:00:00.000Z"),
        totalRunningTime = 1.hour,
        timestamp = Timestamp.parse("2019-05-23T22:22:22.222Z"),
        generation = 7,
        version = "3",
        js7Version = "2.0-JS7",
        buildId = "BUILD"),
      json"""{
        "TYPE": "JS7.Journal",
        "journalId": "ABEiM0RVZneImaq7zN3u_w",
        "eventId": 777,
        "generation": 7,
        "totalEventCount": 999,
        "totalRunningTime": 3600,
        "timestamp": "2019-05-23T22:22:22.222Z",
        "initiallyStartedAt": "2019-05-22T12:00:00Z",
        "version": "3",
        "js7Version": "2.0-JS7",
        "buildId": "BUILD",
        "typeName": "TestState"
      }""")

    // COMPATIBLE with 2.0.0
    testJsonDecoder[JournalHeader](
      JournalHeader(
        typeName = None,
        JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")),
        eventId = EventId(777),
        totalEventCount = 999,
        initiallyStartedAt = Timestamp.parse("2019-05-22T12:00:00.000Z"),
        totalRunningTime = 1.hour,
        timestamp = Timestamp.parse("2019-05-23T22:22:22.222Z"),
        generation = 7,
        version = "3",
        js7Version = "2.0-JS7",
        buildId = "BUILD"),
      json"""{
        "TYPE": "JS7.Journal",
        "journalId": "ABEiM0RVZneImaq7zN3u_w",
        "eventId": 777,
        "generation": 7,
        "totalEventCount": 999,
        "totalRunningTime": 3600,
        "timestamp": "2019-05-23T22:22:22.222Z",
        "startedAt": "2019-05-22T12:00:00Z",
        "version": "3",
        "js7Version": "2.0-JS7",
        "buildId": "BUILD"
      }""")

  "checkedHeader" in:
    val header = JournalHeader(
      typeName = Some("JournalHeaderTest.TestState"),
      JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")),
      eventId = EventId(777),
      totalEventCount = 999,
      initiallyStartedAt = Timestamp.parse("2019-05-22T12:00:00.000Z"),
      totalRunningTime = 1.hour,
      timestamp = Timestamp.parse("2019-05-23T22:22:22.222Z"),
      generation = 7,
      version = "3",
      js7Version = "2.0-JS7",
      buildId = "BUILD")

    val file = Paths.get("FILE")

    assert(
      JournalHeader.checkedHeader[TestState](header.copy(typeName = Some("OTHER")), file, None) ==
        Left(JournalTypeMismatchProblem(file, expected = "JournalHeaderTest.TestState", "OTHER")))

    assert(
      JournalHeader.checkedHeader[TestState](header.copy(typeName = None), file, None) ==
        Right(()))

    assert(
      JournalHeader.checkedHeader[TestState](header, file, None) ==
        Right(()))

    val expectedJournalId = JournalId(UUID.fromString("00000000-000-000-000-000000000000"))
    assert(
      JournalHeader.checkedHeader[TestState](header, file, Some(expectedJournalId)) ==
        Left(JournalIdMismatchProblem(file, expectedJournalId = expectedJournalId, header.journalId)))

    assert(
      JournalHeader.checkedHeader[TestState](header, file, Some(header.journalId)) ==
        Right(()))


object JournalHeaderTest:
  final case class TestState() extends BasicState[TestState]:
    def companion = TestState
  object TestState extends BasicState.Companion[TestState]
