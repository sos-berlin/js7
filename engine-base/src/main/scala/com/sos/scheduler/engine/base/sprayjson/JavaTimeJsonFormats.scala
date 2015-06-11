package com.sos.scheduler.engine.base.sprayjson

import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant}
import spray.json.{JsString, JsValue, JsonFormat}

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
        case _ ⇒ sys.error(s"Instant string expected instead of ${jsValue.getClass.getSimpleName}")
      }
    }

    implicit object DurationJsonFormat extends JsonFormat[Duration] {
      def write(o: Duration) = JsString(o.toString)

      def read(jsValue: JsValue) = jsValue match {
        case JsString(string) ⇒ Duration.parse(string)
        case _ ⇒ sys.error(s"Duration string expected instead of ${jsValue.getClass.getSimpleName}")
      }
    }
  }
}
