package com.sos.jobscheduler.base.circeutils

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.JavaJsonCodecs._
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import java.nio.file.Paths
import java.time.format.DateTimeParseException
import java.time.{Duration, Instant}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaJsonCodecsTest extends FreeSpec {

  "Path" in {
    testJson(Paths.get("/tmp/test"), """ "/tmp/test" """)
  }

  "Instant" - {  // See also Timestamp
    case class A(instant: Instant)

    "String/Number speed comparision" in {

      val millis = Instant.parse("2015-06-09T12:22:33.987Z").toEpochMilli
      val n = 100000
      run("milliseconds")(NumericInstantEncoder, InstantDecoder)
      run("ISO string  ")(StringInstantEncoder, InstantDecoder)

      def run(what: String)(implicit instantJsonCodec: Encoder[Instant], decoder: Decoder[Instant]) = {
        for (i ← 1 to 100000) Instant.ofEpochMilli(i).asJson.as[Instant].force  // Warm-up
        val t = System.currentTimeMillis
        for (i ← 1 to n) Instant.ofEpochMilli(millis + i).asJson.as[Instant].force
        val duration = System.currentTimeMillis - t
        info(s"Instant as $what: ${if (duration > 0) 1000*n / duration else "∞"} conversions/s")
      }
    }

    "String" - {
      import JavaJsonCodecs.instant.StringInstantJsonCodec
      intelliJuseImport(StringInstantJsonCodec)
      implicit val aJsonCodec = deriveCirceCodec[A]

      "No second fraction" in {
        val t = "2015-06-09T12:22:33Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" → Json.fromString(t))
        assert(a.asJson == j)
        assert(a == j.as[A].force)
      }

      "With milliseconds" in {
        val t = "2015-06-09T12:22:33.987Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" → Json.fromString(t))
        assert(a.instant.getNano == 987000000)
        assert(a.asJson == j)
        assert(a == j.as[A].force)
      }

      "With microseconds" in {
        val t = "2015-06-09T12:22:33.987654Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" → Json.fromString(t))
        assert(a.instant.getNano == 987654000)
        assert(a.asJson == j)
        assert(a == j.as[A].force)
      }

      "With nanoseconds" in {
        val t = "2015-06-09T12:22:33.987654321Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" → Json.fromString(t))
        assert(a.instant.getNano == 987654321)
        assert(a.asJson == j)
        assert(a == j.as[A].force)
      }
    }

    "Numeric" - {
      import JavaJsonCodecs.instant.InstantJsonCodec

      "Numeric" in {
        val instant = Instant.parse("2017-11-12T09:24:32.471Z")
        val millis = 1510478672471L
        check(instant,  millis)
      }

      "Numeric before 1970" in {
        check(Instant.parse("1900-01-01T00:00:00Z"), -2208988800000L)
      }

      //"With nanoseconds" in {
      //  val instant = Instant.parse("1900-01-01T00:00:00.123456789Z")
      //  val epochMilli = -2208988800000L + 123
      //  val js = Json.fromInt(BigDecimal(epochMilli) + BigDecimal("0.456789"))
      //  assert(instant.asJson == js)
      //  assert(js.as[Instant].force == instant)
      //}

      "Only millisecond precision" in {
        val instant = Instant.parse("1900-01-01T00:00:00.123456789Z")
        val epochMilli = -2208988800000L + 123
        val js = Json.fromBigDecimal(BigDecimal(epochMilli))
        assert(instant.asJson == js)
        assert(js.as[Instant].force == Instant.ofEpochMilli(instant.toEpochMilli))
      }

      //"Ignoring more precision than milliseconds" in {
      //  assert(Json.fromBigDecimal(BigDecimal("0.987654321")).as[Instant].force == Instant.ofEpochMilli(0))
      //  assert(Json.fromBigDecimal(BigDecimal("0.9")).as[Instant].force == Instant.ofEpochMilli(0))
      //  //intercept[ArithmeticException] { Json.obj("instant" → Json.fromInt(BigDecimal("0.0000009"))).as[A].force }
      //  //intercept[ArithmeticException] { Json.obj("instant" → Json.fromInt(BigDecimal("0.1"))).as[A].force }  // No more precision than milliseconds
      //}

      def check(instant: Instant, epochMilli: Long): Unit = {
        assert(instant.toEpochMilli == epochMilli)
        assert(instant.asJson == Json.fromLong(epochMilli))
        assert(Json.fromLong(epochMilli).as[Instant].force == instant)
      }
    }
  }

  "Duration" - {
    case class A(duration: Duration)
    implicit val aJsonCodec = deriveCirceCodec[A]

    "Seconds" in {
      val a = A(Duration.ofSeconds(3))
      val j = Json.obj("duration" → Json.fromInt(3))
      assert(a.asJson == j)
      assert(a == j.as[A].force)
    }

    "Nanoseconds" in {
      val a = A(Duration.ofNanos(123456789))
      val j = Json.obj("duration" → Json.fromDoubleOrNull(0.123456789))
      assert(a.asJson == j)
      assert(a == j.as[A].force)
    }

    "Invalid syntax" in {
      intercept[DateTimeParseException] { Json.obj("duration" → Json.fromString("1X")).as[A].force }
    }

    "Numeric" in {
      def check(bigDecimal: BigDecimal, duration: Duration) = {
        val json = Json.obj("duration" → Json.fromBigDecimal(bigDecimal))
        assert(json.as[A].force.duration == duration)
        assert(A(duration).asJson == json)
      }
      check(123, Duration.ofSeconds(123))
      check(123.987654321, Duration.ofSeconds(123, 987654321))
      check(BigDecimal("111222333444555666.987654321"), Duration.ofSeconds(111222333444555666L, 987654321))
      intercept[ArithmeticException] { Json.obj("duration" → Json.fromBigDecimal(BigDecimal("0.0000000009"))).as[A].force }
    }
  }
}
