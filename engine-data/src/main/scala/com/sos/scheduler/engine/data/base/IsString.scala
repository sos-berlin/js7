package com.sos.scheduler.engine.data.base

import javax.annotation.Nullable
import scala.language.implicitConversions
import scala.xml.Text
import spray.json.{JsString, JsValue, JsonFormat, JsonWriter}

//@JsonSerialize(using = classOf[IsStringSerializer])
trait IsString extends SerializableIsString {
  def string: String

  final def isEmpty = string.isEmpty

  final def nonEmpty = string.nonEmpty

  override def equals(o: Any) = o match {
    case o: IsString => (getClass eq o.getClass) && string == o.string
    case _ => false
  }

  final override def hashCode = string.hashCode

  override def toString = string
}


object IsString {
  /** Für &lt;elememt attribute={stringValue}/>. */
  implicit def toXmlText(o: StringValue): Text = new xml.Text(o.string)

  @Nullable def stringOrNull[A <: IsString](o: Option[A]): String = o match {
    case Some(a) => a.string
    case None => null
  }

  val UnsupportedJsonDeserialization = (o: String) ⇒
    throw new UnsupportedOperationException("JSON deserialization not supported for this (abstract?) IsString")

  class MyJsonWriter[A <: IsString] extends JsonWriter[A] {
    final def write(o: A) = JsString(o.string)
  }

  final class MyJsonFormat[A <: IsString](construct: String => A) extends MyJsonWriter[A] with JsonFormat[A] {
    def read(jsValue: JsValue): A = jsValue match {
      case JsString(string) ⇒ construct(string)
      case _ => sys.error(s"String expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }

  trait HasJsonFormat[A <: IsString] {
    def apply(o: String): A

    implicit val MyJsonFormat = new IsString.MyJsonFormat(apply)
  }
}
