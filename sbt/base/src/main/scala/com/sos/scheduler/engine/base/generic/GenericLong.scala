package com.sos.scheduler.engine.base.generic

import spray.json.{JsNumber, JsValue, JsonFormat, JsonWriter}

/**
 * @author Joacim Zschimmer
 */
trait GenericLong {
  val number: Long
}

object GenericLong {
//  val UnsupportedJsonDeserialization = (o: Long) ⇒
//    throw new UnsupportedOperationException("JSON deserialization not supported for this (abstract?) GenericLong")

  class MyJsonWriter[A <: GenericLong] extends JsonWriter[A] {
    final def write(o: A) = JsNumber(o.number)
  }

  final class MyJsonFormat[A <: GenericLong](construct: Long ⇒ A) extends MyJsonWriter[A] with JsonFormat[A] {
    def read(jsValue: JsValue): A = jsValue match {
      case JsNumber(o) ⇒ construct(o.toLongExact)
      case _ => sys.error(s"Number (64bit) expected instead of ${jsValue.getClass.getSimpleName}")
    }
  }

  trait HasJsonFormat[A <: GenericLong] {
    def apply(o: Long): A
    implicit val MyJsonFormat = new GenericLong.MyJsonFormat(apply)
  }
}
