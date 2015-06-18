package com.sos.scheduler.engine.agent.web.marshal

import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import java.lang.StringBuilder
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import spray.json._

/**
 * A printer for YAML, likely inefficient for big objects.
 */
object YamlPrinter extends JsonPrinter {

  private val yaml = {
    val options = new DumperOptions sideEffect  { o ⇒
      o.setDefaultFlowStyle(FlowStyle.BLOCK)
      o.setWidth(160)
    }
    new Yaml(options)
  }

  def print(v: JsValue, sb: StringBuilder) = sb.append(yaml.dump(yaml.load(v.compactPrint)))
}
