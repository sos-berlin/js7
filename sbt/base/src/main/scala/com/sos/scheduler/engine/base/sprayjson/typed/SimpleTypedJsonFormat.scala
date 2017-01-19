package com.sos.scheduler.engine.base.sprayjson.typed

import spray.json.{JsObject, JsString, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait SimpleTypedJsonFormat[A] extends TypedJsonFormat[A] {

  protected def typeName: String
  protected def subclasses: Set[Class[_ <: A]]

  protected def typelessWrite(a: A): JsObject

  protected val typeField = TypedJsonFormat.DefaultTypeFieldName → JsString(typeName)

  final def canSerialize(e: A) = subclasses contains e.getClass

  final lazy val classToJsonWriter = (subclasses map { _ → this }).toMap[Class[_], RootJsonWriter[_]]

  final def typeNameToJsonReader = Map(typeName → this)

  final val typeNameToClass: Map[String, Class[_ <: A]] = Map()

  final val subtypeNames = Nil

  final def write(a: A) = {
    val jsObject = typelessWrite(a)
    JsObject(jsObject.fields + typeField)
  }
}
