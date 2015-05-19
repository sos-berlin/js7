package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import java.nio.file.{Path, Paths}
import spray.json.{JsString, JsValue, JsonFormat}

/**
 * @author Joacim Zschimmer
 */
object SprayJson {
  object implicits {

    implicit object PathJsonFormat extends JsonFormat[Path] {
      def write(o: Path) = JsString(o.toString)

      def read(o: JsValue) = Paths.get(cast[JsString](o).value)
    }
  }
}
