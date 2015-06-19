package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import scala.collection.JavaConversions._
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

  def toYaml(v: JsValue) = yaml.dump(yaml.load(v.compactPrint))

  def yamlToJsValue(yamlString: String): JsValue = valueToJsValue(yaml.load(yamlString))

  private def valueToJsValue(value: Any): JsValue =
    value match {
      case v: String ⇒ JsString(v)
      case v: Boolean ⇒ JsBoolean(v)
      case v: Integer ⇒ JsNumber(v)
      case v: Short ⇒ JsNumber(v.toInt)
      case v: Long ⇒ JsNumber(v)
      case v: Float ⇒ JsNumber(v.toDouble)
      case v: Double ⇒ JsNumber(v)
      case null ⇒ JsNull
      case v: java.math.BigDecimal ⇒ JsNumber(v)
      case v: java.lang.Iterable[_] ⇒ JsArray((v map valueToJsValue).toVector)
      case v: java.util.Map[_, _] ⇒ mapToJsObject(v.asInstanceOf[java.util.Map[String, Any]])
    }

  private def mapToJsObject(m: java.util.Map[String, Any]): JsObject =
    JsObject(m.entrySet.toSeq map { e ⇒ e.getKey → valueToJsValue(e.getValue) }: _*)
}
