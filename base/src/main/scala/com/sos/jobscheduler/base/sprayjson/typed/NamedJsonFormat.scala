package com.sos.jobscheduler.base.sprayjson.typed

import com.sos.jobscheduler.base.sprayjson.typed.TypedJsonFormat.DefaultTypeFieldName
import spray.json.{JsObject, JsString, JsValue, RootJsonFormat}

/**
  * @author Joacim Zschimmer
  */
final class NamedJsonFormat[A](val typeName: String, delegate: RootJsonFormat[A])
extends RootJsonFormat[A]{

  private val typeJsString = JsString(typeName)
  private val typeField = DefaultTypeFieldName → typeJsString

  def write(a: A) = {
    delegate.write(a).asJsObject
    //jsObject.fields.get("TYPE") match {
    //  case Some(`typeJsString`) ⇒ jsObject
    //  case None ⇒ JsObject(jsObject.fields + typeField)
    //  case Some(o) ⇒ sys.error(s"Serialization type name mismatch: TYPE=$o, but it should be $typeJsString")
    //}
  }

  def read(json: JsValue) = delegate.read(json)
}

object NamedJsonFormat {
  implicit class ToTypeJsonFormat[A](val delegate: RootJsonFormat[A]) extends AnyVal {
    def withTypeName(typeName: String): NamedJsonFormat[A] =
      new NamedJsonFormat(typeName, delegate)
  }
}
