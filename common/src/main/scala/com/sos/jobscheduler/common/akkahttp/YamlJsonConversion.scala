package com.sos.jobscheduler.common.akkahttp

import com.sos.jobscheduler.base.sprayjson.SprayJson.valueToJsValue
import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import org.yaml.snakeyaml.DumperOptions.FlowStyle
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import scala.collection.JavaConversions._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object YamlJsonConversion {

  implicit class ToYamlString[A](val delegate: A) extends AnyVal {
    /**
      * Converts this with spray-json to YAML.
      */
    def toYamlString(implicit writer: JsonWriter[A]): String = YamlJsonConversion.toYamlString(delegate.toJson)

    /**
      * Converts this with spray-json to YAML using flow styles.
      * This should return a one-line string.
      */
    def toFlowYamlString(implicit writer: JsonWriter[A]): String = YamlJsonConversion.toYamlString(delegate.toJson, FlowStyle.FLOW).trim
  }

  def toYamlString(v: JsValue): String =
    yaml.dump(jsValueToYaml(v))

  def toYamlString(v: JsValue, flowStyle: FlowStyle): String =
    yaml.dumpAs(yaml.load(v.compactPrint), Tag.MAP, flowStyle)

  def jsValueToYaml(v: JsValue): Any =
    jsValueToJava(v)
    //Safe, but slow: yaml.load(v.compactPrint)

  def yamlToJsValue(yamlString: String): JsValue =
    valueToJsValue(yaml.load(yamlString))

  def yaml: Yaml = {
    val options = new DumperOptions sideEffect { o ⇒
      o.setDefaultFlowStyle(FlowStyle.BLOCK)
      o.setWidth(100)
    }
    new Yaml(options)
  }

  private def jsValueToJava(jsValue: JsValue): Any =
    jsValue match {
      case JsString(o) ⇒ o: String
      case JsBoolean(o) ⇒ o: Boolean
      case JsNull ⇒ null
      case JsNumber(o) ⇒
        try o.toLongExact
        catch { case _: ArithmeticException ⇒ o: BigDecimal }
      case JsArray(o) ⇒ o map jsValueToJava: java.util.List[Any]
      case o: JsObject ⇒ jsObjectToMap(o)
    }

  private def jsObjectToMap(jsObject: JsObject): java.util.Map[String, Any] =
    jsObject.fields map { case (k, v) ⇒ k → jsValueToJava(v) }
}
