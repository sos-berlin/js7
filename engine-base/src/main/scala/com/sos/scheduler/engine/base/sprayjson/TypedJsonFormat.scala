package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat._
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicits.toJavaFunction
import java.util.concurrent.ConcurrentHashMap
import scala.reflect.ClassTag
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class TypedJsonFormat[Super, Sub <: Super] private(
  commonSuperClass: Class[Super],
  classToLazyWriter: Map[Class[_ <: Sub], LazyWriteEntry],
  typeToLazyReader: Map[String, () ⇒ RootJsonReader[_]],
  typeFieldName: String,
  shortenTypeOnlyValue: Boolean)
extends RootJsonFormat[Sub] {

  private val classToWriter = new ConcurrentHashMap[Class[_ <: Sub], WriteEntry]
  private val typeToReader = new ConcurrentHashMap[String, RootJsonReader[_]]

  def write(o: Sub) = {
    val w: WriteEntry = classToWriter.computeIfAbsent(o.getClass, { clazz: Class[_ <: Sub] ⇒
      val e = classToLazyWriter.getOrElse(clazz,
        throw new NoSuchElementException(s"${o.getClass} is not serializable as subclass of ${commonSuperClass.getName}"))
      WriteEntry(e.getJsonWriter(), typeFieldName → JsString(e.typeName))
    })
    val serializedFields = w.jsonWriter.write(o).asJsObject.fields
    if (shortenTypeOnlyValue && serializedFields.isEmpty)
      w.typeField._2   // To be short, typename-only values are serialized as simple string "typename"
    else
      JsObject(serializedFields + w.typeField)
  }

  def read(jsValue: JsValue) = {
    val (typeName, fields) = jsValue match {
      case o: JsString ⇒ (o.value, Map.empty[String, JsValue])
      case o: JsObject ⇒ (o.fields(typeFieldName).asInstanceOf[JsString].value, o.fields - typeFieldName)
      case o ⇒ throw new IllegalArgumentException(s"Expected JSON string or object for type ${commonSuperClass.getSimpleName}")
    }
    val reader = typeToReader.computeIfAbsent(typeName, { typeName: String ⇒
      val getReader = typeToLazyReader.getOrElse(typeName,
        throw new NoSuchElementException(s"""JSON $typeFieldName="$typeName" denotes an unknown subtype of ${commonSuperClass.getSimpleName}"""))
      getReader()
    })
    reader.read(JsObject(fields)).asInstanceOf[Sub]
  }
}

object TypedJsonFormat {
  private val DefaultTypeFieldName = "TYPE"
  private val DefaultShortenTypeOnlyValue = true

  // On compile error using TypedJsonFormat(), check if every Subtype really denotes a subtype of A.
  def apply[A: ClassTag](subtypes: Subtype[_ <: A]*): TypedJsonFormat[A, A] =
    _apply[A](subtypes = subtypes)

  // On compile error using TypedJsonFormat(), check if every Subtype really denotes a subtype of A.
  def apply[A: ClassTag](
    typeField: String = DefaultTypeFieldName,
    shortenTypeOnlyValue: Boolean = DefaultShortenTypeOnlyValue)
    (subtypes: Subtype[_ <: A]*): TypedJsonFormat[A, A]
  =
    _apply[A](typeFieldName = typeField, shortenTypeOnlyValue = shortenTypeOnlyValue, subtypes = subtypes)

  private def _apply[A: ClassTag](
    typeFieldName: String = DefaultTypeFieldName,
    shortenTypeOnlyValue: Boolean = DefaultShortenTypeOnlyValue,
    subtypes: Seq[Subtype[_ <: A]]): TypedJsonFormat[A, A]
  =
    new TypedJsonFormat[A, A](
      implicitClass[A],
      (subtypes map { o ⇒ o.clazz → LazyWriteEntry(() ⇒ o.lazyJsonFormat().asInstanceOf[RootJsonWriter[Any]], o.typeName) }).toMap,
      (subtypes map { o ⇒ o.typeName → (() ⇒ o.lazyJsonFormat()) }).toMap,
      typeFieldName = typeFieldName,
      shortenTypeOnlyValue = shortenTypeOnlyValue)

  /**
    * @param lazyJsonFormat lazy to allow recursive tree structure with not yet initialized implicit commonSuperClass jsonFormat.
    */
  final case class Subtype[A](clazz: Class[A], lazyJsonFormat: () ⇒ RootJsonFormat[A], typeName: String)

  object Subtype {
    def apply[A: ClassTag: RootJsonFormat]: Subtype[A] =
      Subtype[A](implicitly[RootJsonFormat[A]])

    def apply[A: ClassTag: RootJsonFormat](typeName: String): Subtype[A] =
      new Subtype(implicitClass[A], () ⇒ implicitly[RootJsonFormat[A]], typeName)

    def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
      new Subtype(implicitClass[A], () ⇒ jsonFormat, implicitClass[A].getSimpleName stripSuffix "$")

    def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A], typeName: String): Subtype[A] =
      new Subtype(implicitClass[A], () ⇒ jsonFormat, typeName)
  }

  final case class LazyWriteEntry(getJsonWriter: () ⇒ RootJsonWriter[Any], typeName: String)

  final case class WriteEntry(jsonWriter: RootJsonWriter[Any], typeField: (String, JsString))
}
