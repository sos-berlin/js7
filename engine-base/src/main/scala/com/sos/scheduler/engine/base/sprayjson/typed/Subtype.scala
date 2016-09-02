package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.utils.ScalaUtils._
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

  def apply[A: ClassTag: RootJsonFormat](typeName: String): Subtype[A] =
    new SingleSubtype[A](
      implicitClass[A],
      typeName,
      jsonFormat = implicitly[RootJsonFormat[A]])

  def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
    new SingleSubtype[A](
      implicitClass[A],
      name = implicitClass[A].getSimpleName stripSuffix "$",
      jsonFormat)

  def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A], typeName: String): Subtype[A] =
    new SingleSubtype[A](
      implicitClass[A],
      typeName,
      jsonFormat)
}

final case class SingleSubtype[A](clazz: Class[_ <: A], name: String, jsonFormat: RootJsonFormat[A])
extends Subtype[A] {

  def nameToClass =
    Map(name → clazz) ++
      (jsonFormat match {
        case o: WithSubtypeRegister[A @unchecked] ⇒  // Recursive TypedJsonFormat
          require(o.superclass isAssignableFrom clazz)
          o.typeToClass
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
        o.typeToJsonReader
      case o: HasOwnTypeField[A @unchecked] ⇒
        o.typeToJsonReader mapValues { _ ⇒ jsonFormat }
      case _ ⇒
        Map(name → jsonFormat)
    }
}

final case class MultipleSubtype[A](classes: Set[Class[_ <: A]], jsonFormat: RootJsonFormat[A])
extends Subtype[A] {

  def nameToClass =
    jsonFormat match {
      case o: WithSubtypeRegister[A @unchecked] ⇒  // Recursive TypedJsonFormat
        require(classes forall o.superclass.isAssignableFrom)
        o.typeToClass
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
        o.typeToJsonReader
      case o: HasOwnTypeField[A @unchecked] ⇒
        o.typeToJsonReader mapValues { _ ⇒ jsonFormat }
    }
}
