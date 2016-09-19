package com.sos.scheduler.engine.base.sprayjson.typed

import scala.collection.immutable
import spray.json._

/**
  * @author Joacim Zschimmer
  */
private[typed] class WithSubtypeRegister[A](
  val superclass: Class[A],
  val classToJsonWriter: Map[Class[_], RootJsonWriter[_]],
  val typeNameToClass: Map[String, Class[_ <: A]],
  val typeNameToJsonReader: Map[String, RootJsonReader[_]],
  val subtypeNames: immutable.Seq[String],
  typeFieldName: String,
  shortenTypeOnlyValue: Boolean)
extends TypedJsonFormat[A] {

  override def asJsObjectJsonFormat =
    new WithSubtypeRegister[A](superclass, classToJsonWriter,
      typeNameToClass, typeNameToJsonReader, subtypeNames, typeFieldName,
      shortenTypeOnlyValue = false)

  override def canSerialize(a: A): Boolean =
    classToJsonWriter.get(a.getClass) match {
      case Some(h: CanSerialize[A @unchecked]) ⇒ h canSerialize a
      case Some(h: TypedJsonFormat[A @unchecked]) ⇒ h canSerialize a
      case Some(_) ⇒ true
      case _ ⇒ false
    }

  def write(a: A) = {
    val jsonWriter = classToJsonWriter.getOrElse(a.getClass,
        sys.error(s"${a.getClass} is not registered as subclass in TypedJsonFormat[${superclass.getName}]"))
      .asInstanceOf[JsonWriter[A]]
    val result = jsonWriter.write(a).asJsObject
    if (shortenTypeOnlyValue && result.fields.keySet == Set(typeFieldName))
      result.fields.head._2.asInstanceOf[JsString]   // Shorten type only value to JsString("type")
    else
      result
  }

  def read(jsValue: JsValue) = {
    val (typeName, fields: Map[String, JsValue]) = jsValue match {
      case o: JsString ⇒ (o.value, Map.empty)
      case o: JsObject ⇒ (o.fields(typeFieldName).asInstanceOf[JsString].value, o.fields /*- typeFieldName*/)
      case o ⇒ sys.error(s"Expected JSON string or object for type ${superclass.getSimpleName}")
    }
    val reader = typeNameToJsonReader.getOrElse(typeName,
      sys.error(s"""JSON $typeFieldName="$typeName" does not denote a subtype of ${superclass.getSimpleName}"""))
    reader.read(JsObject(fields)).asInstanceOf[A]
  }
}
