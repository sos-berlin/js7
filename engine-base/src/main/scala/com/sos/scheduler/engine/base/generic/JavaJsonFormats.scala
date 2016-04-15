package com.sos.scheduler.engine.base.generic

import java.io.File
import spray.json.{JsString, JsValue, JsonFormat}

/**
  * @author Joacim Zschimmer
  */
object JavaJsonFormats {
  implicit object FileJsonFormat extends JsonFormat[File] {
    def write(o: File) = JsString(o.getPath)

    def read(jsValue: JsValue) = jsValue match {
      case JsString(string) ⇒ new File(string)
      case _ => sys.error(s"File string expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }
}
