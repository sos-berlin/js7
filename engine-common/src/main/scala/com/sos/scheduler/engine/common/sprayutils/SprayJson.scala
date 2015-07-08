package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import java.nio.file.{Path, Paths}
import scala.collection.JavaConversions._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object SprayJson {
  def valueToJsValue(value: Any): JsValue =
    value match {
      case v: String ⇒ JsString(v)
      case v: Boolean ⇒ JsBoolean(v)
      case v: Integer ⇒ JsNumber(v)
      case v: Short ⇒ JsNumber(v.toInt)
      case v: Long ⇒ JsNumber(v)
      case v: Float ⇒ JsNumber(v.toDouble)
      case v: Double ⇒ JsNumber(v)
      case null ⇒ JsNull
      case v: Map[_, _] ⇒ mapToJsObject(v.asInstanceOf[Map[String, Any]])
      case v: JsValue ⇒ v
      case v: java.math.BigDecimal ⇒ JsNumber(v)
      case v: java.lang.Iterable[_] ⇒ JsArray((v map valueToJsValue).toVector)
      case v: java.util.Map[_, _] ⇒ mapToJsObject(v.asInstanceOf[java.util.Map[String, Any]])
    }

  def mapToJsObject(m: java.util.Map[String, Any]): JsObject =
    JsObject(m.entrySet.toSeq map { e ⇒ e.getKey → valueToJsValue(e.getValue) }: _*)

  object implicits {
    implicit object AnyMapJsonFormat extends RootJsonFormat[Map[String, Any]] {
      def write(o: Map[String, Any]): JsObject = mapToJsObject(o)
      def read(json: JsValue) = throw new UnsupportedOperationException("Map[String, Any] is not deserializable")  // So AnyMapJsonFormat can be used in jsonFormat()
    }

    implicit object PathJsonFormat extends JsonFormat[Path] {
      def write(o: Path) = JsString(o.toString)

      def read(o: JsValue) = Paths.get(cast[JsString](o).value)
    }
  }
}
