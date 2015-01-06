package com.sos.scheduler.engine.data.base

/**
 * @author Joacim Zschimmer
 */

import java.io.File
import spray.json.{JsValue, JsString, JsonFormat}

object JavaJsonFormats {
  implicit object FileJsonFormat extends JsonFormat[File] {
    def write(o: File) = JsString(o.getPath)

    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒ new File(string)
      case _ => sys.error(s"File string expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }
}
