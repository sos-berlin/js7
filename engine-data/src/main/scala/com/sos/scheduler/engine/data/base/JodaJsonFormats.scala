package com.sos.scheduler.engine.data.base

import org.joda.time.Instant
import org.joda.time.format.ISODateTimeFormat
import spray.json.{JsValue, JsString, JsonFormat}

/**
 * @author Joacim Zschimmer
 */
object JodaJsonFormats {
  implicit object InstantJsonFormat extends JsonFormat[Instant] {
    private val format = ISODateTimeFormat.dateTime

    def write(o: Instant) = JsString(format print o)

    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒ (format parseDateTime string).toInstant
      case _ => sys.error(s"Instant string expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }
}
