package com.sos.jobscheduler.common.akkahttp

import com.sos.jobscheduler.common.akkahttp.YamlJsonConversion.toYamlString
import java.lang.StringBuilder
import spray.json._

/**
 * A printer for YAML, likely inefficient for big objects.
 */
object YamlPrinter extends JsonPrinter {

  def print(v: JsValue, sb: StringBuilder) = sb.append(toYamlString(v))
}
