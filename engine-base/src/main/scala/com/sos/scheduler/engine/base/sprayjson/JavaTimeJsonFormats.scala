package com.sos.scheduler.engine.base.sprayjson

import java.time.Instant
import java.time.format.DateTimeFormatter
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
  }
}
