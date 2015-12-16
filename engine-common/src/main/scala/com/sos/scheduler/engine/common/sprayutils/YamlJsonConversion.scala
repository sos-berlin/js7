package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.sprayutils.SprayJson.valueToJsValue
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object YamlJsonConversion {

  private val yaml = {
    val options = new DumperOptions sideEffect { o ⇒
      o.setDefaultFlowStyle(FlowStyle.BLOCK)
      o.setWidth(100)
    }
    new Yaml(options)
  }

  def toYaml(v: JsValue): String = yaml.dump(yaml.load(v.compactPrint))

  def yamlToJsValue(yamlString: String): JsValue = valueToJsValue(yaml.load(yamlString))

  implicit class ToYamlString[A](val delegate: A) extends AnyVal {
    def toYamlString(implicit writer: JsonWriter[A]): String = toYaml(delegate.toJson)
  }
}
