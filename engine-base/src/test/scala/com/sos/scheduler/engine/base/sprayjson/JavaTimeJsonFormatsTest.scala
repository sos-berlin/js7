package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.format.DateTimeParseException
import java.time.{Duration, Instant}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JavaTimeJsonFormatsTest extends FreeSpec {

  "Instant" - {
    case class A(instant: Instant)
    implicit val aJsonFormat = jsonFormat1(A.apply)

    "Instant" in {
      val t = "2015-06-09T12:22:33Z"
      val a = A(Instant.parse(t))
      val j = JsObject("instant" → JsString(t))
      assert(a.toJson == j)
      assert(a == j.convertTo[A])
    }

    "Instant with milliseconds" in {
      val t = "2015-06-09T12:22:33.987Z"
      val a = A(Instant.parse(t))
      val j = JsObject("instant" → JsString(t))
      assert(a.instant.getNano == 987000000)
      assert(a.toJson == j)
      assert(a == j.convertTo[A])
    }

    "Instant with microseconds" in {
      val t = "2015-06-09T12:22:33.987654Z"
      val a = A(Instant.parse(t))
      val j = JsObject("instant" → JsString(t))
      assert(a.instant.getNano == 987654000)
      assert(a.toJson == j)
      assert(a == j.convertTo[A])
    }

    "Instant with nanoseconds" in {
      val t = "2015-06-09T12:22:33.987654321Z"
      val a = A(Instant.parse(t))
      val j = JsObject("instant" → JsString(t))
      assert(a.instant.getNano == 987654321)
      assert(a.toJson == j)
      assert(a == j.convertTo[A])
    }
  }

  "Duration" - {
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
