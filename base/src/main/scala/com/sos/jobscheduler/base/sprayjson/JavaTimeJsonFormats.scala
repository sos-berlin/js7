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
    def write(o: Instant) = JsNumber(o.toEpochMilli)
  }

  trait InstantJsonReader extends JsonReader[Instant] {
    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒ Instant.from(formatter parse string)
      case JsNumber(number) ⇒ millisToInstant(number.toLong)
      case _ ⇒ sys.error(s"Instant string expected instead of ${jsValue.getClass.getSimpleName}")
    }

    private def millisToInstant(millis: Long) =
      Instant.ofEpochSecond(millis / 1000, millis % 1000 * 1000 * 1000)
  }

  object implicits {
    val NumericInstantJsonFormat: JsonFormat[Instant] = new JsonFormat[Instant] with NumericInstantJsonWriter with InstantJsonReader
    val StringInstantJsonFormat: JsonFormat[Instant] = new JsonFormat[Instant] with StringInstantJsonWriter with InstantJsonReader
    implicit val InstantJsonFormat = StringInstantJsonFormat

    implicit val DurationJsonFormat = new JsonFormat[Duration] {
      def write(o: Duration) = if (o.getNano == 0) JsNumber(o.getSeconds) else JsNumber(durationToBigDecimal(o))

      def read(jsValue: JsValue) = jsValue match {
        case JsString(string) ⇒ Duration.parse(string)
        case JsNumber(bigDecimal) ⇒ bigDecimalToDuration(bigDecimal)
        case _ ⇒ sys.error(s"Duration string expected instead of ${jsValue.getClass.getSimpleName}")
      }

      private def bigDecimalToDuration(o: BigDecimal) = {
        val (seconds, nanos) = o /% 1
        Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)
      }

      private def durationToBigDecimal(o: Duration) = BigDecimal(o.getSeconds) + BigDecimal(o.getNano, scale = 9)
    }
  }
}
