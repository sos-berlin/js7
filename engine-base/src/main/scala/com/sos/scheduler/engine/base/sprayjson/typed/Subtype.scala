package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.utils.ScalaUtils._
import scala.language.existentials
import scala.reflect.ClassTag
import spray.json.{JsString, RootJsonFormat, RootJsonReader, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
trait Subtype[A] {
  def nameToClass: Map[String, Class[_ <: A]]
  private[typed] def toClassToJsonWriter(typeFieldName: String): Map[Class[_], RootJsonWriter[_]]
  private[typed] def toTypeToReader(typeFieldName: String): Map[String, RootJsonReader[_]]
}

object Subtype {
  def apply[A: ClassTag: RootJsonFormat]: Subtype[A] =
    Subtype[A](implicitly[RootJsonFormat[A]])

  def apply[A: ClassTag: RootJsonFormat](name: String): Subtype[A] =
    Subtype(implicitly[RootJsonFormat[A]], name = name)

  def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
    Subtype(jsonFormat, name = implicitClass[A].getSimpleName stripSuffix "$")

  def apply[A: ClassTag](lazyJsonFormat: ⇒ RootJsonFormat[A], name: String): Subtype[A] = {
    val jsonFormat = lazyJsonFormat
    jsonFormat match {
      case jsonFormat: TypedJsonFormat[A] ⇒
        new MultipleSubtype[A](jsonFormat.classes, jsonFormat)
      case _ ⇒
        new SingleSubtype[A](implicitClass[A], name, jsonFormat)
    }
  }
}

final case class SingleSubtype[A](clazz: Class[_ <: A], name: String, jsonFormat: RootJsonFormat[A])
extends Subtype[A] {

  def nameToClass =
    Map(name → clazz) ++
      (jsonFormat match {
        case o: WithSubtypeRegister[A @unchecked] ⇒  // Recursive TypedJsonFormat
          require(o.superclass isAssignableFrom clazz)
          o.typeNameToClass
        case _ ⇒
          Map()
      })

  private[typed] def toClassToJsonWriter(typeFieldName: String): Map[Class[_], RootJsonWriter[_]] =
    jsonFormat match {
      case o: WithSubtypeRegister[A @unchecked] ⇒  // Recursive TypedJsonFormat
        require(o.superclass isAssignableFrom clazz)
        o.classToJsonWriter
      case _ ⇒
        Map(
          clazz → new SingleTypeJsonWriter[A](typeFieldName → JsString(name), jsonFormat.asInstanceOf[RootJsonWriter[A]]))
    }

  private[typed] def toTypeToReader(typeFieldName: String): Map[String, RootJsonReader[_]] =
    jsonFormat match {
      case o: WithSubtypeRegister[A @unchecked] ⇒
        require(o.superclass isAssignableFrom clazz)
        o.typeNameToJsonReader
      case o: HasOwnTypeField[A @unchecked] ⇒
        o.typeNameToJsonReader mapValues { _ ⇒ jsonFormat }
      case _ ⇒
        Map(name → jsonFormat)
    }
}

final case class MultipleSubtype[A](classes: Set[Class[_ <: A]], jsonFormat: RootJsonFormat[A])
extends Subtype[A] {

  def nameToClass =
    jsonFormat match {
      case o: WithSubtypeRegister[A @unchecked] ⇒  // Recursive TypedJsonFormat
        for (c ← classes) require(o.superclass isAssignableFrom c, s"$c is not a subclass of ${o.superclass}")
        o.typeNameToClass
      case _ ⇒
        Map()
    }

  private[typed] def toClassToJsonWriter(typeFieldName: String): Map[Class[_], RootJsonWriter[_]] =
    jsonFormat match {
      case o: WithSubtypeRegister[A @unchecked] ⇒  // Recursive TypedJsonFormat
        require(classes forall o.superclass.isAssignableFrom)
        o.classToJsonWriter
      case _ ⇒
        val w = jsonFormat.asInstanceOf[RootJsonWriter[A]]
        (classes map { _ → w }).toMap
    }

  private[typed] def toTypeToReader(typeFieldName: String): Map[String, RootJsonReader[_]] =
    jsonFormat match {
      case o: WithSubtypeRegister[A @unchecked] ⇒
        require(classes forall o.superclass.isAssignableFrom)
        o.typeNameToJsonReader
      case o: HasOwnTypeField[A @unchecked] ⇒
        o.typeNameToJsonReader mapValues { _ ⇒ jsonFormat }
    }
}
