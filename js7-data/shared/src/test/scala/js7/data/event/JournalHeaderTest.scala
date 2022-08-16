package js7.data.event

import java.util.UUID
import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class JournalHeaderTest extends OurTestSuite
{
  "JSON" in {
    testJson[JournalHeader](
      JournalHeader(
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
        "buildId": "BUILD"
      }""")

    // COMPATIBLE with 2.0.0
    testJsonDecoder[JournalHeader](
      JournalHeader(
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
  }
}
