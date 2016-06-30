package com.sos.scheduler.engine.base.sprayjson

import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant}
import spray.json.{JsNumber, JsString, JsValue, JsonFormat}

/**
 * @author Joacim Zschimmer
 */
object JavaTimeJsonFormats {

  object implicits {

    implicit object InstantJsonFormat extends JsonFormat[Instant] {
      private val formatter = DateTimeFormatter.ISO_INSTANT

      def write(o: Instant) = JsString(formatter.format(o))

      def read(jsValue: JsValue) = jsValue match {
        case JsString(string) ⇒ Instant.from(formatter parse string)
        case JsNumber(number) ⇒ bigDecimalToInstant(number.bigDecimal)
        case _ ⇒ sys.error(s"Instant string expected instead of ${jsValue.getClass.getSimpleName}")
      }
    }

    implicit object DurationJsonFormat extends JsonFormat[Duration] {
      def write(o: Duration) = JsNumber(durationToBigDecimal(o))

      def read(jsValue: JsValue) = jsValue match {
        case JsString(string) ⇒ Duration.parse(string)
        case JsNumber(bigDecimal) ⇒ bigDecimalToDuration(bigDecimal)
        case _ ⇒ sys.error(s"Duration string expected instead of ${jsValue.getClass.getSimpleName}")
      }
    }
  }

  private def bigDecimalToDuration(o: BigDecimal) = {
    val (seconds, nanos) = o /% 1
    Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)
  }

  private def durationToBigDecimal(o: Duration) = BigDecimal(o.getSeconds) + BigDecimal(o.getNano, scale = 9)

  private def bigDecimalToInstant(o: BigDecimal) = {
    val (seconds, nanos) = o /% 1
    Instant.ofEpochSecond(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)
  }
}
