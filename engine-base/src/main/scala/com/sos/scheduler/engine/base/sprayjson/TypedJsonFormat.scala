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
  typeFieldName: String = DefaultTypeFieldName)
extends RootJsonFormat[Sub] {

  private val classToWriter = new ConcurrentHashMap[Class[_ <: Sub], WriteEntry]
  private val typeToReader = new ConcurrentHashMap[String, RootJsonReader[_]]

  def withTypeFieldName(name: String) =
    new TypedJsonFormat[Super, Sub](commonSuperClass, classToLazyWriter, typeToLazyReader, typeFieldName = name)

  def write(o: Sub) = {
    val w: WriteEntry = classToWriter.computeIfAbsent(o.getClass, { clazz: Class[_ <: Sub] ⇒
      val e = classToLazyWriter.getOrElse(clazz,
        throw new NoSuchElementException(s"${o.getClass} is not serializable as subclass of ${commonSuperClass.getName}"))
      WriteEntry(e.getJsonWriter(), typeFieldName → JsString(e.typeName))
    })
    val serializedFields = w.jsonWriter.write(o).asJsObject.fields
    if (serializedFields.isEmpty)
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

  def apply[A: ClassTag](formats: Subtype[_ <: A]*) =
    new TypedJsonFormat[A, A](
      implicitClass[A],
      (formats map { o ⇒ o.clazz → LazyWriteEntry(() ⇒ o.lazyJsonFormat().asInstanceOf[RootJsonWriter[Any]], o.typeName) }).toMap,
      (formats map { o ⇒ o.typeName → (() ⇒ o.lazyJsonFormat()) }).toMap)

  /**
    * @param lazyJsonFormat lazy to allow recursive tree structure with not yet initialized implicit commonSuperClass jsonFormat.
    */
  final case class Subtype[A](clazz: Class[A], typeName: String, lazyJsonFormat: () ⇒ RootJsonFormat[A])

  object Subtype {
    def apply[A: ClassTag: RootJsonFormat]: Subtype[A] =
      Subtype[A](implicitly[RootJsonFormat[A]])

    def apply[A: ClassTag: RootJsonFormat](typeName: String): Subtype[A] =
      new Subtype(implicitClass[A], typeName, () ⇒ implicitly[RootJsonFormat[A]])

    def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
      new Subtype(implicitClass[A], implicitClass[A].getSimpleName stripSuffix "$", () ⇒ jsonFormat)

    def apply[A: ClassTag](typeName: String, jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
      new Subtype(implicitClass[A], typeName, () ⇒ jsonFormat)
  }

  final case class LazyWriteEntry(getJsonWriter: () ⇒ RootJsonWriter[Any], typeName: String)

  final case class WriteEntry(jsonWriter: RootJsonWriter[Any], typeField: (String, JsString))
}
