package com.sos.scheduler.engine.base.generic

import com.sos.scheduler.engine.base.convert.As
import javax.annotation.Nullable
import scala.language.implicitConversions
import spray.json.{JsString, JsValue, JsonFormat, JsonWriter}

trait IsString {
  def string: String

  final def isEmpty = string.isEmpty

  final def nonEmpty = string.nonEmpty

  final override def hashCode = string.hashCode ^ getClass.hashCode

  override def toString = string
}

object IsString {
  /** Für &lt;elememt attribute={stringValue}/>. */
  implicit def toXmlText(o: IsString): xml.Text = if (o == null) null else xml.Text(o.string)

  @Nullable def stringOrNull[A <: IsString](o: Option[A]): String = o match {
    case Some(a) ⇒ a.string
    case None ⇒ null
  }

  val UnsupportedJsonDeserialization = (o: String) ⇒
    throw new UnsupportedOperationException("JSON deserialization not supported for this (abstract?) IsString")

  class MyJsonWriter[A <: IsString] extends JsonWriter[A] {
    final def write(o: A) = JsString(o.string)
  }

  final class MyJsonFormat[A <: IsString](construct: String => A) extends MyJsonWriter[A] with JsonFormat[A] {
    def read(jsValue: JsValue): A = jsValue match {
      case JsString(string) ⇒ construct(string)
      case _ ⇒ sys.error(s"String expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }

  trait HasJsonFormat[A <: IsString] {
    def apply(o: String): A

    implicit val MyJsonFormat = new IsString.MyJsonFormat(apply)
  }

  trait Companion[A <: IsString] extends HasJsonFormat[A] {
    implicit val ordering: Ordering[A] = Ordering by { _.string }
    implicit val IsStringAsString: As[String, A] = As(apply)
  }
}
