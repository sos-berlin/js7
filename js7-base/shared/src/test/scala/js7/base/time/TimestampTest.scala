package js7.base.time

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.*
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class TimestampTest extends OurTestSuite:
  private val isoString = "2017-12-04T11:22:33.456Z"
  private val millis = 1512386553456L
  private val timestamp = Timestamp(isoString)

  "JSON" in:
    testJson(Timestamp.ofEpochMilli(millis), json"""1512386553456""")
    //testJson(Timestamp.ofEpochMilli(millis), json""" "2017-12-04T11:22:33.456Z" """)

  "ofEpochMilli" in:
    assert(timestamp.toEpochMilli == millis)
    assert(Timestamp.ofEpochMilli(millis) == timestamp)

  "ofEpochSecond" in:
    val seconds = 1512386553L
    assert(timestamp.toEpochSecond == seconds)
    assert(Timestamp.ofEpochSecond(seconds) == Timestamp("2017-12-04T11:22:33Z"))

  "toIsoString" in:
    assert(timestamp.toIsoString == isoString)

  "toTimeString" in:
    assert(timestamp.toTimeString == "11:22:33.456")
    assert(Timestamp("2021-10-08T11:22:33Z").toTimeString == "11:22:33")
    assert(Timestamp("2021-10-08T11:22:00Z").toTimeString == "11:22:00")

  "plus" in:
    assert(timestamp + (1.hour + 111.milliseconds) == Timestamp("2017-12-04T12:22:33.567Z"))
    assert(timestamp + Long.MaxValue.nanoseconds == Timestamp("2310-03-16T11:09:50.310Z"))

  "minus" in:
    assert(Timestamp("2017-12-04T12:22:33.567Z") - timestamp == 1.hour + 111.milliseconds)
    assert(Timestamp("2017-12-04T12:22:33.567Z") - 1.minute == Timestamp("2017-12-04T12:21:33.567Z") )

  "min" in:
    assert(Timestamp("2018-11-21T12:00:00Z").min(Timestamp("2019-11-21T12:00:00Z")) == Timestamp("2018-11-21T12:00:00Z"))

  "max" in:
    assert(Timestamp("2018-11-21T12:00:00Z").max(Timestamp("2019-11-21T12:00:00Z")) == Timestamp("2019-11-21T12:00:00Z"))

  "secondsOnly" in:
    assert(Timestamp("2021-01-17T12:11:22.345Z").secondsOnly ==
           Timestamp("2021-01-17T12:11:22Z"))
    assert(Timestamp("2021-01-17T12:11:22Z").secondsOnly ==
           Timestamp("2021-01-17T12:11:22Z"))

  "roundToNextSecond" in:
    assert(Timestamp("2019-03-07T12:00:00Z").roundToNextSecond == Timestamp("2019-03-07T12:00:00Z"))
    assert(Timestamp("2019-03-07T12:00:00.001Z").roundToNextSecond == Timestamp("2019-03-07T12:00:01Z"))
    assert(Timestamp("2019-03-07T12:00:00.999Z").roundToNextSecond == Timestamp("2019-03-07T12:00:01Z"))

  "roundTo" in:
    assert(Timestamp("2015-01-01T12:01:01.499Z").roundTo(1.second) == Timestamp("2015-01-01T12:01:01Z"))
    assert(Timestamp("2015-01-01T12:01:01.500Z").roundTo(1.second) == Timestamp("2015-01-01T12:01:02Z"))
    assert(Timestamp("2015-01-01T12:01:29.999Z").roundTo(60.seconds) == Timestamp("2015-01-01T12:01:00Z"))
    assert(Timestamp("2015-01-01T12:01:30Z"    ).roundTo(60.seconds) == Timestamp("2015-01-01T12:02:00Z"))

  "roundDownTo" in:
    assert(Timestamp("2015-01-01T12:01:01.499Z").roundDownTo(1.second) == Timestamp("2015-01-01T12:01:01Z"))
    assert(Timestamp("2015-01-01T12:01:01.500Z").roundDownTo(1.second) == Timestamp("2015-01-01T12:01:01Z"))
    assert(Timestamp("2015-01-01T12:01:29.999Z").roundDownTo(60.seconds) == Timestamp("2015-01-01T12:01:00Z"))
    assert(Timestamp("2015-01-01T12:01:30Z"    ).roundDownTo(60.seconds) == Timestamp("2015-01-01T12:01:00Z"))

  "show" in:
    assert(Timestamp("2018-11-21T12:34:56Z").show == "2018-11-21T12:34:56Z")
    assert(Timestamp("2018-11-21T12:34:56.987Z").show == "2018-11-21T12:34:56.987Z")

  "pretty" in:
    assert(Timestamp("2018-11-21T12:34:56Z").pretty == "2018-11-21 12:34:56Z")
    assert(Timestamp("2018-11-21T12:34:56.987Z").pretty == "2018-11-21 12:34:56.987Z")

  if sys.props contains "test.speed" then
  "JSON String/Number speed comparision" in:
    val n = 100000
    run("milliseconds")(using Timestamp.implementation.NumericTimestampJsonEncoder, Timestamp.jsonDecoder)
    run("ISO string  ")(using Timestamp.implementation.StringTimestampJsonEncoder, Timestamp.jsonDecoder)

    def run(what: String)(implicit encoder: Encoder[Timestamp], decoder: Decoder[Timestamp]) =
      for i <- 1 to 100000 do Timestamp.ofEpochMilli(i).asJson.as[Timestamp].orThrow  // Warm-up
      val t = System.currentTimeMillis
      for i <- 1 to n do Timestamp.ofEpochMilli(millis + i).asJson.as[Timestamp].orThrow
      val duration = System.currentTimeMillis - t
      info(s"Timestamp as $what: ${if duration > 0 then 1000*n / duration else "∞"} conversions/s")
