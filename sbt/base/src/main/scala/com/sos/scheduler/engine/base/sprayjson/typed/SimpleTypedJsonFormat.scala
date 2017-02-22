package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.sprayjson.typed.TypedJsonFormat.DefaultTypeFieldName
import spray.json.{JsObject, JsString, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait SimpleTypedJsonFormat[A] extends TypedJsonFormat[A] {

  protected def typeName: String
  protected def subclasses: Set[Class[_ <: A]]

  protected def typelessWrite(a: A): JsObject

  private val typeNameJs = JsString(typeName)
  protected val typeField = DefaultTypeFieldName → typeNameJs

  final def canSerialize(e: A) = subclasses contains e.getClass

  final def canDeserialize(o: JsObject) = o.fields.get(DefaultTypeFieldName) contains typeNameJs

  final lazy val classToJsonWriter = (subclasses map { _ → this }).toMap[Class[_], RootJsonWriter[_]]

  final def typeNameToJsonReader = Map(typeName → this)

  final val typeNameToClass: Map[String, Class[_ <: A]] = Map()

  final val subtypeNames = Nil

  final def write(a: A) = {
    val jsObject = typelessWrite(a)
    JsObject(jsObject.fields + typeField)
  }
}
