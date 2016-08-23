package com.sos.scheduler.engine.base.sprayjson.typed

import spray.json.{JsObject, JsString, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait SimpleTypedJsonFormat[A] extends TypedJsonFormat[A] {

  protected def subclasses: Set[Class[_ <: A]]

  protected def typeField: (String, JsString)

  protected def typelessWrite(a: A): JsObject

  final def canSerialize(e: A) = subclasses contains e.getClass

  final lazy val classToJsonWriter = (subclasses map { _ → this }).toMap[Class[_], RootJsonWriter[_]]

  final def typeToJsonReader = Map(typeName → this)

  final def typeName: String = typeField._2.value

  final def write(a: A) = {
    val jsObject = typelessWrite(a)
    JsObject(jsObject.fields + typeField)
  }
}
