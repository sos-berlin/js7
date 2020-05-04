package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import java.time.Instant
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericTimestampTest extends AnyFreeSpec
{
  private val isoString = "2017-12-04T11:22:33.456Z"
  private val millis = 1512386553456L
  private val timestamp = Timestamp.parse(isoString)

  "JSON" in {
    testJson(Timestamp.ofEpochMilli(millis), json"""1512386553456""")
    //testJson(Timestamp.ofEpochMilli(millis), json""" "2017-12-04T11:22:33.456Z" """)
  }

  "ofEpochMilli" in {
    assert(timestamp.toEpochMilli == millis)
    assert(Timestamp.ofEpochMilli(millis) == timestamp)
  }

  "ofEpochSecond" in {
    val seconds = 1512386553L
    assert(timestamp.toEpochSecond == seconds)
    assert(Timestamp.ofEpochSecond(seconds) == Timestamp.parse("2017-12-04T11:22:33Z"))
  }

  "toIsoString" in {
    assert(timestamp.toIsoString == isoString)
  }

  "plus" in {
    assert(timestamp + (1.hour + 111.milliseconds) == Timestamp.parse("2017-12-04T12:22:33.567Z"))
  }

  "minus" in {
    assert(Timestamp.parse("2017-12-04T12:22:33.567Z") - timestamp == 1.hour + 111.milliseconds)
    assert(Timestamp.parse("2017-12-04T12:22:33.567Z") - 1.minute == Timestamp.parse("2017-12-04T12:21:33.567Z") )
  }

  "min" in {
    assert(Timestamp.parse("2018-11-21T12:00:00Z").min(Timestamp.parse("2019-11-21T12:00:00Z")) == Timestamp.parse("2018-11-21T12:00:00Z"))
  }

  "max" in {
    assert(Timestamp.parse("2018-11-21T12:00:00Z").max(Timestamp.parse("2019-11-21T12:00:00Z")) == Timestamp.parse("2019-11-21T12:00:00Z"))
  }

  "roundToNextSecond" in {
    assert(Timestamp.parse("2019-03-07T12:00:00Z").roundToNextSecond == Timestamp.parse("2019-03-07T12:00:00Z"))
    assert(Timestamp.parse("2019-03-07T12:00:00.001Z").roundToNextSecond == Timestamp.parse("2019-03-07T12:00:01Z"))
    assert(Timestamp.parse("2019-03-07T12:00:00.999Z").roundToNextSecond == Timestamp.parse("2019-03-07T12:00:01Z"))
  }

  "roundTo" in {
    assert(Timestamp.parse("2015-01-01T12:01:01.499Z").roundTo(1.second) == Timestamp.parse("2015-01-01T12:01:01Z"))
    assert(Timestamp.parse("2015-01-01T12:01:01.500Z").roundTo(1.second) == Timestamp.parse("2015-01-01T12:01:02Z"))
    assert(Timestamp.parse("2015-01-01T12:01:29.999Z").roundTo(60.seconds) == Timestamp.parse("2015-01-01T12:01:00Z"))
    assert(Timestamp.parse("2015-01-01T12:01:30Z"    ).roundTo(60.seconds) == Timestamp.parse("2015-01-01T12:02:00Z"))
  }

  "roundDownTo" in {
    assert(Timestamp.parse("2015-01-01T12:01:01.499Z").roundDownTo(1.second) == Timestamp.parse("2015-01-01T12:01:01Z"))
    assert(Timestamp.parse("2015-01-01T12:01:01.500Z").roundDownTo(1.second) == Timestamp.parse("2015-01-01T12:01:01Z"))
    assert(Timestamp.parse("2015-01-01T12:01:29.999Z").roundDownTo(60.seconds) == Timestamp.parse("2015-01-01T12:01:00Z"))
    assert(Timestamp.parse("2015-01-01T12:01:30Z"    ).roundDownTo(60.seconds) == Timestamp.parse("2015-01-01T12:01:00Z"))
  }

  "show" in {
    assert(Timestamp.parse("2018-11-21T12:34:56Z").show == "2018-11-21T12:34:56Z")
    assert(Timestamp.parse("2018-11-21T12:34:56.987Z").show == "2018-11-21T12:34:56.987Z")
  }

  "pretty" in {
    assert(Timestamp.parse("2018-11-21T12:34:56Z").pretty == "2018-11-21 12:34:56Z")
    assert(Timestamp.parse("2018-11-21T12:34:56.987Z").pretty == "2018-11-21 12:34:56.987Z")
  }

  "ofDeadline" in {
    val a = Deadline.now + 10.seconds
    val ts = Timestamp.ofDeadline(a)
    assert(ts >= Timestamp.now + 9.seconds &&
           ts <= Timestamp.now + 11.seconds)
  }

  if (sys.props contains "test.speed")
  "JSON String/Number speed comparision" in {
    val n = 100000
    run("milliseconds")(Timestamp.NumericTimestampJsonEncoder, Timestamp.jsonDecoder)
    run("ISO string  ")(Timestamp.StringTimestampJsonEncoder, Timestamp.jsonDecoder)

    def run(what: String)(implicit encoder: Encoder[Timestamp], decoder: Decoder[Timestamp]) = {
      for (i <- 1 to 100000) Timestamp.ofEpochMilli(i).asJson.as[Timestamp].orThrow  // Warm-up
      val t = System.currentTimeMillis
      for (i <- 1 to n) Timestamp.ofEpochMilli(millis + i).asJson.as[Timestamp].orThrow
      val duration = System.currentTimeMillis - t
      info(s"Timestamp as $what: ${if (duration > 0) 1000*n / duration else "∞"} conversions/s")
    }
  }
}
