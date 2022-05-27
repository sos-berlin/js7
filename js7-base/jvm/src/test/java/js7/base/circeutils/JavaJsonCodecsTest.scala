package js7.base.circeutils

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import java.io.File
import java.nio.file.Paths
import java.time.format.DateTimeParseException
import java.time.{Duration, Instant}
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.JavaJsonCodecs._
import js7.base.time.ScalaTime._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class JavaJsonCodecsTest extends AnyFreeSpec {

  "Path" in {
    if (File.separatorChar == '\\') {
      testJson(Paths.get("/tmp/test"), json""" "\\tmp\\test" """)
    } else {
      testJson(Paths.get("/tmp/test"), json""" "/tmp/test" """)
    }
  }

  "Instant" - {  // See also Timestamp
    case class A(instant: Instant)

    if (sys.props contains "test.speed")
    "String/Number speed comparision" in {
      val millis = Instant.parse("2015-06-09T12:22:33.987Z").toEpochMilli
      val n = 100000
      run("milliseconds")(NumericInstantEncoder, InstantDecoder)
      run("ISO string  ")(StringInstantEncoder, InstantDecoder)

      def run(what: String)(implicit instantJsonCodec: Encoder[Instant], decoder: Decoder[Instant]) = {
        for (i <- 1 to 100000) Instant.ofEpochMilli(i).asJson.as[Instant].orThrow  // Warm-up
        val t = System.nanoTime
        for (i <- 1 to n) Instant.ofEpochMilli(millis + i).asJson.as[Instant].orThrow
        val duration = (System.nanoTime - t).nanoseconds
        info(s"Instant as $what: ${if (duration.isPositive) 1000*n / duration.toMillis else "∞"} conversions/s")
      }
    }

    "String" - {
      import JavaJsonCodecs.instant.StringInstantJsonCodec
      intelliJuseImport(StringInstantJsonCodec)
      implicit val aJsonCodec = deriveCodec[A]

      "No second fraction" in {
        val t = "2015-06-09T12:22:33Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" -> Json.fromString(t))
        assert(a.asJson == j)
        assert(a == j.as[A].orThrow)
      }

      "With milliseconds" in {
        val t = "2015-06-09T12:22:33.987Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" -> Json.fromString(t))
        assert(a.instant.getNano == 987000000)
        assert(a.asJson == j)
        assert(a == j.as[A].orThrow)
      }

      "With microseconds" in {
        val t = "2015-06-09T12:22:33.987654Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" -> Json.fromString(t))
        assert(a.instant.getNano == 987654000)
        assert(a.asJson == j)
        assert(a == j.as[A].orThrow)
      }

      "With nanoseconds" in {
        val t = "2015-06-09T12:22:33.987654321Z"
        val a = A(Instant.parse(t))
        val j = Json.obj("instant" -> Json.fromString(t))
        assert(a.instant.getNano == 987654321)
        assert(a.asJson == j)
        assert(a == j.as[A].orThrow)
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
      //  assert(js.as[Instant].orThrow == instant)
      //}

      "Only millisecond precision" in {
        val instant = Instant.parse("1900-01-01T00:00:00.123456789Z")
        val epochMilli = -2208988800000L + 123
        val js = Json.fromBigDecimal(BigDecimal(epochMilli))
        assert(instant.asJson == js)
        assert(js.as[Instant].orThrow == Instant.ofEpochMilli(instant.toEpochMilli))
      }

      //"Ignoring more precision than milliseconds" in {
      //  assert(Json.fromBigDecimal(BigDecimal("0.987654321")).as[Instant].orThrow == Instant.ofEpochMilli(0))
      //  assert(Json.fromBigDecimal(BigDecimal("0.9")).as[Instant].orThrow == Instant.ofEpochMilli(0))
      //  //intercept[ArithmeticException] { Json.obj("instant" -> Json.fromInt(BigDecimal("0.0000009"))).as[A].orThrow }
      //  //intercept[ArithmeticException] { Json.obj("instant" -> Json.fromInt(BigDecimal("0.1"))).as[A].orThrow }  // No more precision than milliseconds
      //}

      def check(instant: Instant, epochMilli: Long): Unit = {
        assert(instant.toEpochMilli == epochMilli)
        assert(instant.asJson == Json.fromLong(epochMilli))
        assert(Json.fromLong(epochMilli).as[Instant].orThrow == instant)
      }
    }
  }

  "Duration" - {
    case class A(duration: Duration)
    implicit val aJsonCodec = deriveCodec[A]

    "Seconds" in {
      val a = A(Duration.ofSeconds(3))
      val j = Json.obj("duration" -> Json.fromInt(3))
      assert(a.asJson == j)
      assert(a == j.as[A].orThrow)
    }

    "Nanoseconds" in {
      val a = A(Duration.ofNanos(123456789))
      val j = Json.obj("duration" -> Json.fromDoubleOrNull(0.123456789))
      assert(a.asJson == j)
      assert(a == j.as[A].orThrow)
    }

    "Invalid syntax" in {
      intercept[DateTimeParseException] { Json.obj("duration" -> Json.fromString("1X")).as[A].orThrow }
    }

    "Numeric" in {
      def check(bigDecimal: BigDecimal, duration: Duration) = {
        val json = Json.obj("duration" -> Json.fromBigDecimal(bigDecimal))
        assert(json.as[A].orThrow.duration == duration)
        assert(A(duration).asJson == json)
      }
      check(123, Duration.ofSeconds(123))
      check(123.987654321, Duration.ofSeconds(123, 987654321))
      check(BigDecimal("111222333444555666.987654321"), Duration.ofSeconds(111222333444555666L, 987654321))
      intercept[ArithmeticException] { Json.obj("duration" -> Json.fromBigDecimal(BigDecimal("0.0000000009"))).as[A].orThrow }
    }
  }
}
