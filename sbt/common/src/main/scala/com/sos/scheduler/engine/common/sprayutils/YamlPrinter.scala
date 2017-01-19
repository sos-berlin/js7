package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.YamlJsonConversion.toYaml
import java.lang.StringBuilder
import spray.json._

/**
 * A printer for YAML, likely inefficient for big objects.
 */
object YamlPrinter extends JsonPrinter {

  def print(v: JsValue, sb: StringBuilder) = sb.append(toYaml(v))
}
