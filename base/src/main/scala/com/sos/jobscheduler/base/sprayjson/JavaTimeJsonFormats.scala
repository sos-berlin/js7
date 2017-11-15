package com.sos.jobscheduler.base.sprayjson

import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant}
import spray.json.{JsNumber, JsString, JsValue, JsonFormat, JsonReader, JsonWriter}

/**
 * @author Joacim Zschimmer
 */
object JavaTimeJsonFormats {

  private val formatter = DateTimeFormatter.ISO_INSTANT

  trait StringInstantJsonWriter extends JsonWriter[Instant] {
    def write(o: Instant) = JsString(formatter.format(o))
  }

  trait NumericInstantJsonWriter extends JsonWriter[Instant] {
    def write(o: Instant) =
      JsNumber(o.toEpochMilli)
      //With nanosecond precision:
      //o.getNano % (1000*1000) match {
      //  case 0 ⇒
      //  case n ⇒ JsNumber(BigDecimal(o.toEpochMilli) + BigDecimal(n, 6))  // Slow
      //}
  }

  trait InstantJsonReader extends JsonReader[Instant] {
    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒
        Instant.from(formatter parse string)
      case JsNumber(number) ⇒
        val millis = number.toLong
        Instant.ofEpochSecond(millis / 1000, millis % 1000 * 1000 * 1000)

      case _ ⇒ sys.error(s"Instant string expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }

  val NumericInstantJsonFormat: JsonFormat[Instant] = new JsonFormat[Instant] with NumericInstantJsonWriter with InstantJsonReader
  val StringInstantJsonFormat: JsonFormat[Instant] = new JsonFormat[Instant] with StringInstantJsonWriter with InstantJsonReader

  object implicits {
    implicit val InstantJsonFormat = StringInstantJsonFormat

    implicit val DurationJsonFormat = new JsonFormat[Duration] {
      def write(o: Duration) =
        if (o.getNano == 0)
          JsNumber(o.getSeconds)
        else
          JsNumber(BigDecimal(o.getSeconds) + BigDecimal(o.getNano, scale = 9))

      def read(jsValue: JsValue) = jsValue match {
        case JsString(string) ⇒
          Duration.parse(string)

        case JsNumber(bigDecimal) ⇒
          val (seconds, nanos) = bigDecimal /% 1
          Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)

        case _ ⇒ sys.error(s"Duration string expected instead of ${jsValue.getClass.getSimpleName}")
      }
    }
  }
}
