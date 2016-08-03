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
  typeToLazyReader: Map[String, () ⇒ RootJsonReader[_]])
extends RootJsonFormat[Sub] {

  private val classToWriter = new ConcurrentHashMap[Class[_ <: Sub], WriteEntry]
  private val typeToReader = new ConcurrentHashMap[String, RootJsonReader[_]]

  def write(o: Sub) = {
    val w: WriteEntry = classToWriter.computeIfAbsent(o.getClass, { clazz: Class[_ <: Sub] ⇒
      val e = classToLazyWriter.getOrElse(clazz,
        throw new NoSuchElementException(s"${o.getClass} is not serializable as subclass of ${commonSuperClass.getName}"))
      WriteEntry(e.getJsonWriter(), e.typeField)
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
      case o: JsObject ⇒ (o.fields(TypeFieldName).asInstanceOf[JsString].value, o.fields - TypeFieldName)
      case o ⇒ throw new IllegalArgumentException(s"Expected JSON string or object for type ${commonSuperClass.getSimpleName}")
    }
    val reader = typeToReader.computeIfAbsent(typeName, { typeName: String ⇒
      val getReader = typeToLazyReader.getOrElse(typeName,
        throw new NoSuchElementException(s"""JSON $TypeFieldName="$typeName" denotes an unknown subtype of ${commonSuperClass.getSimpleName}"""))
      getReader()
    })
    reader.read(JsObject(fields)).asInstanceOf[Sub]
  }
}

object TypedJsonFormat {
  val TypeFieldName = "TYPE"

  def apply[A: ClassTag](formats: Subtype[_ <: A]*) =
    new TypedJsonFormat[A, A](
      implicitClass[A],
      (formats map { o ⇒ o.clazz → LazyWriteEntry(() ⇒ o.lazyJsonFormat().asInstanceOf[RootJsonWriter[Any]], TypeFieldName → JsString(o.typeName)) }).toMap,
      (formats map { o ⇒ o.typeName → (() ⇒ o.lazyJsonFormat()) }).toMap)

  /**
    * @param lazyJsonFormat lazy to allow recursive tree structure with not yet initialized implicit commonSuperClass jsonFormat.
    */
  final case class Subtype[A](clazz: Class[A], typeName: String, lazyJsonFormat: () ⇒ RootJsonFormat[A])

  object Subtype {
    def apply[A : ClassTag](jsonFormat: RootJsonFormat[A]): Subtype[A] =
      Subtype(implicitClass[A], implicitClass[A].getSimpleName stripSuffix "$", () ⇒ jsonFormat)

    def apply[A : ClassTag](typeName: String, jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
      Subtype(implicitClass[A], typeName, () ⇒ jsonFormat)
  }

  final case class LazyWriteEntry(getJsonWriter: () ⇒ RootJsonWriter[Any], typeField: (String, JsString))

  final case class WriteEntry(jsonWriter: RootJsonWriter[Any], typeField: (String, JsString))
}
