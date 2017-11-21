package com.sos.jobscheduler.base.sprayjson

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats._
import java.time.format.DateTimeParseException
import java.time.{Duration, Instant}
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class JavaTimeJsonFormatsTest extends FreeSpec {

  "Instant" - {
    case class A(instant: Instant)


    "String/Number speed comparision" in {
      val instant = Instant.parse("2015-06-09T12:22:33.987Z")
      val n = 100000
      run("milliseconds")(new JsonFormat[Instant] with NumericInstantJsonWriter with InstantJsonReader)
      run("ISO string  ")(new JsonFormat[Instant] with StringInstantJsonWriter with InstantJsonReader)

      def run(what: String)(implicit InstantJsonFormat: JsonFormat[Instant]) = {
        for (_ ← 1 to 100000) instant.toJson.convertTo[Instant]  // Warm-up
        val t = System.currentTimeMillis
        for (_ ← 1 to n) instant.toJson.convertTo[Instant]
        val duration = System.currentTimeMillis - t
        info(s"Instant as $what: ${if (duration > 0) 1000*n / duration else "∞"} conversions/s")
      }
    }


    "String" - {
      implicit val instantJsonFormat = StringInstantJsonFormat  // Override default implicit JsonFormat
      implicit val aJsonFormat = jsonFormat1(A.apply)

      "No second fraction" in {
        val t = "2015-06-09T12:22:33Z"
        val a = A(Instant.parse(t))
        val j = JsObject("instant" → JsString(t))
        assert(a.toJson == j)
        assert(a == j.convertTo[A])
      }

      "With milliseconds" in {
        val t = "2015-06-09T12:22:33.987Z"
        val a = A(Instant.parse(t))
        val j = JsObject("instant" → JsString(t))
        assert(a.instant.getNano == 987000000)
        assert(a.toJson == j)
        assert(a == j.convertTo[A])
      }

      "With microseconds" in {
        val t = "2015-06-09T12:22:33.987654Z"
        val a = A(Instant.parse(t))
        val j = JsObject("instant" → JsString(t))
        assert(a.instant.getNano == 987654000)
        assert(a.toJson == j)
        assert(a == j.convertTo[A])
      }

      "With nanoseconds" in {
        val t = "2015-06-09T12:22:33.987654321Z"
        val a = A(Instant.parse(t))
        val j = JsObject("instant" → JsString(t))
        assert(a.instant.getNano == 987654321)
        assert(a.toJson == j)
        assert(a == j.convertTo[A])
      }
    }

    "Numeric" - {
      implicit def InstantJsonFormat = JavaTimeJsonFormats.NumericInstantJsonFormat  // Override default

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
      //  val js = JsNumber(BigDecimal(epochMilli) + BigDecimal("0.456789"))
      //  assert(instant.toJson == js)
      //  assert(js.convertTo[Instant] == instant)
      //}

      "Only millisecond precision" in {
        val instant = Instant.parse("1900-01-01T00:00:00.123456789Z")
        val epochMilli = -2208988800000L + 123
        val js = JsNumber(BigDecimal(epochMilli))
        assert(instant.toJson == js)
        assert(js.convertTo[Instant] == Instant.ofEpochMilli(instant.toEpochMilli))
      }

      "Ignoring more precision than milliseconds" in {
        assert(JsNumber(BigDecimal("0.0000009")).convertTo[Instant] == Instant.ofEpochMilli(0))
        assert(JsNumber(BigDecimal("0.1")).convertTo[Instant] == Instant.ofEpochMilli(0))
        //intercept[ArithmeticException] { JsObject("instant" → JsNumber(BigDecimal("0.0000009"))).convertTo[A] }
        //intercept[ArithmeticException] { JsObject("instant" → JsNumber(BigDecimal("0.1"))).convertTo[A] }  // No more precision than milliseconds
      }

      def check(instant: Instant, epochMilli: Long): Unit = {
        assert(instant.toEpochMilli == epochMilli)
        assert(instant.toJson == JsNumber(epochMilli))
        assert(JsNumber(epochMilli).convertTo[Instant] == instant)
      }
    }
  }

  "Duration" - {
    import JavaTimeJsonFormats.implicits._

    case class A(duration: Duration)
    implicit val aJsonFormat = jsonFormat1(A.apply)

    "Seconds" in {
      val a = A(Duration.ofSeconds(3))
      val j = JsObject("duration" → JsNumber(3))
      assert(a.toJson == j)
      assert(a == j.convertTo[A])
    }

    "Nanoseconds" in {
      val a = A(Duration.ofNanos(123456789))
      val j = JsObject("duration" → JsNumber(0.123456789))
      assert(a.toJson == j)
      assert(a == j.convertTo[A])
    }

    "Invalid syntax" in {
      intercept[DateTimeParseException] { JsObject("duration" → JsString("1")).convertTo[A] }
    }

    "Numeric" in {
      def check(bigDecimal: BigDecimal, duration: Duration) = {
        val json = JsObject("duration" → JsNumber(bigDecimal))
        assert(json.convertTo[A].duration == duration)
        assert(A(duration).toJson == json)
      }
      check(123, Duration.ofSeconds(123))
      check(123.987654321, Duration.ofSeconds(123, 987654321))
      check(BigDecimal("111222333444555666.987654321"), Duration.ofSeconds(111222333444555666L, 987654321))
      intercept[ArithmeticException] { JsObject("duration" → JsNumber(BigDecimal("0.0000000009"))).convertTo[A] }
    }
  }
}
