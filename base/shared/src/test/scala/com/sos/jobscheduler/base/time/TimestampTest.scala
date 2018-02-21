package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TimestampTest extends FreeSpec {

  private val isoString = "2017-12-04T11:22:33.456Z"
  private val millis = 1512386553456L
  private val timestamp = Timestamp.parse(isoString)

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

  "JSON" in {
    testJson(Timestamp.ofEpochMilli(millis), json"""1512386553456""")
    //testJson(Timestamp.ofEpochMilli(millis), json""" "2017-12-04T11:22:33.456Z" """)
  }

  if (sys.props contains "test.speed")
  "JSON String/Number speed comparision" in {
    val n = 100000
    run("milliseconds")(Timestamp.NumericTimestampJsonEncoder, Timestamp.jsonDecoder)
    run("ISO string  ")(Timestamp.StringTimestampJsonEncoder, Timestamp.jsonDecoder)

    def run(what: String)(implicit encoder: Encoder[Timestamp], decoder: Decoder[Timestamp]) = {
      for (i ← 1 to 100000) Timestamp.ofEpochMilli(i).asJson.as[Timestamp].force  // Warm-up
      val t = System.currentTimeMillis
      for (i ← 1 to n) Timestamp.ofEpochMilli(millis + i).asJson.as[Timestamp].force
      val duration = System.currentTimeMillis - t
      info(s"Timestamp as $what: ${if (duration > 0) 1000*n / duration else "∞"} conversions/s")
    }
  }
}
