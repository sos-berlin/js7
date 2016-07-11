package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.base.sprayjson.SprayJson
import com.sos.scheduler.engine.base.sprayjson.SprayJson.valueToJsValue
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.nodes.Tag
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

  def toYaml(v: JsValue, flowStyle: FlowStyle): String = yaml.dumpAs(yaml.load(v.compactPrint), Tag.MAP, flowStyle)

  def yamlToJsValue(yamlString: String): JsValue = valueToJsValue(yaml.load(yamlString))

  implicit class ToYamlString[A](val delegate: A) extends AnyVal {
    /**
      * Converts this with spray-json to YAML.
      */
    def toYaml(implicit writer: JsonWriter[A]): String = YamlJsonConversion.toYaml(delegate.toJson)

    /**
      * Converts this with spray-json to YAML using flow styles.
      * This should return a one-line string.
      */
    def toFlowYaml(implicit writer: JsonWriter[A]): String = YamlJsonConversion.toYaml(delegate.toJson, FlowStyle.FLOW).trim
  }
}
