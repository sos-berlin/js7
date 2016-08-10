package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat._
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.collection.immutable
import scala.collection.immutable.Iterable
import scala.reflect.ClassTag
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class TypedJsonFormat[A] private(
  private[TypedJsonFormat] val commonSuperClass: Class[A],
  private[TypedJsonFormat] val classToWriteEntry: Map[Class[_ <: A], WriteEntry],
  private[TypedJsonFormat] val typeToReader: Map[String, RootJsonReader[_]],
  typeFieldName: String,
  shortenTypeOnlyValue: Boolean)
extends RootJsonFormat[A] {

  private val serializableClasses: Set[Class[_ <: A]] = classToWriteEntry.keySet

  def canSerialize(a: A) = canSerializeClass(a.getClass)

  def canSerializeClass(c: Class[_ <: A]): Boolean = serializableClasses contains c

  def write(o: A) = {
    val WriteEntry(jsonWriter, typeField) = classToWriteEntry.getOrElse(o.getClass,
      sys.error(s"${o.getClass} is not registered as subclass in TypedJsonFormat[${commonSuperClass.getName}]"))
    val serializedFields = jsonWriter.write(o).asJsObject.fields
    if (shortenTypeOnlyValue && serializedFields.isEmpty)
      typeField._2   // To be short and more readable, typename-only values are serialized as simple string "typename" instead of { "TYPE": "typename" }
    else
      JsObject(serializedFields + typeField)
  }

  def read(jsValue: JsValue) = {
    val (typeName, fields) = jsValue match {
      case o: JsString ⇒ (o.value, Map.empty[String, JsValue])
      case o: JsObject ⇒ (o.fields(typeFieldName).asInstanceOf[JsString].value, o.fields - typeFieldName)
      case o ⇒ throw new IllegalArgumentException(s"Expected JSON string or object for type ${commonSuperClass.getSimpleName}")
    }
    val reader = typeToReader.getOrElse(typeName,
      sys.error(s"""JSON $typeFieldName="$typeName" denotes an unknown subtype of ${commonSuperClass.getSimpleName}"""))
    reader.read(JsObject(fields)).asInstanceOf[A]
  }
}

object TypedJsonFormat {
  private val DefaultTypeFieldName = "TYPE"
  private val DefaultShortenTypeOnlyValue = true

/**
  * A RootJsonType for polymorphic types.
  * <p><b>On compile error using TypedJsonFormat(Subtype ...):</b> check if every Subtype really denotes a subtype of A.
  *
  */
  def apply[A: ClassTag](subtype: Subtype[_ <: A], subtypes: Subtype[_ <: A]*): TypedJsonFormat[A] =
    _apply[A](subtypes = subtype +: subtypes)

  /**
    * A RootJsonType for polymorphic types.
    * <p><b>On compile error using TypedJsonFormat(Subtype ...):</b> check if every Subtype really denotes a subtype of A.
    *
    * @param typeField            Name of the subtype discriminating field
    * @param shortenTypeOnlyValue To be short and more readable,
    *                             typename-only values are serialized as simple string "typename"
    *                             instead of { "TYPE": "typename" }
    * @param subtypes
    * @tparam A
    * @return
    */
  def apply[A: ClassTag](
    typeField: String = DefaultTypeFieldName,
    shortenTypeOnlyValue: Boolean = DefaultShortenTypeOnlyValue)
    (subtypes: Subtype[_ <: A]*): TypedJsonFormat[A]
  =
    _apply[A](typeFieldName = typeField, shortenTypeOnlyValue = shortenTypeOnlyValue, subtypes = subtypes)

  private def _apply[A: ClassTag](
    typeFieldName: String = DefaultTypeFieldName,
    shortenTypeOnlyValue: Boolean = DefaultShortenTypeOnlyValue,
    subtypes: Seq[Subtype[_ <: A]]): TypedJsonFormat[A]
  =
    new TypedJsonFormat[A](
      implicitClass[A],
      (subtypes flatMap { _.toClassToWriteEntry(typeFieldName) }).toMap,
      (subtypes flatMap { _.toTypeToReader(typeFieldName) }).toMap,
      typeFieldName = typeFieldName,
      shortenTypeOnlyValue = shortenTypeOnlyValue)

  final case class Subtype[A](clazz: Class[A], jsonFormat: RootJsonFormat[A], typeName: String) {

    private[TypedJsonFormat] def toClassToWriteEntry(typeFieldName: String): immutable.Iterable[(Class[_ <: A], WriteEntry)] =
      jsonFormat match {
        case o: TypedJsonFormat[A @unchecked] ⇒
          require(o.commonSuperClass isAssignableFrom clazz)
          o.classToWriteEntry
        case _ ⇒
          Vector(clazz → WriteEntry(jsonFormat.asInstanceOf[RootJsonWriter[Any]], typeFieldName → JsString(typeName)))
      }

    private[TypedJsonFormat] def toTypeToReader(typeFieldName: String): Iterable[(String, RootJsonReader[_])] =
      jsonFormat match {
        case o: TypedJsonFormat[A @unchecked] ⇒
          require(o.commonSuperClass isAssignableFrom clazz)
          o.typeToReader
        case _ ⇒
          Vector(typeName → jsonFormat)
      }
  }

  object Subtype {
    def apply[A: ClassTag: RootJsonFormat]: Subtype[A] =
      Subtype[A](implicitly[RootJsonFormat[A]])

    def apply[A: ClassTag: RootJsonFormat](typeName: String): Subtype[A] =
      new Subtype(implicitClass[A], implicitly[RootJsonFormat[A]], typeName)

    def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A]): Subtype[A] =
      new Subtype(implicitClass[A], jsonFormat, implicitClass[A].getSimpleName stripSuffix "$")

    def apply[A: ClassTag](jsonFormat: ⇒ RootJsonFormat[A], typeName: String): Subtype[A] =
      new Subtype(implicitClass[A], jsonFormat, typeName)
  }

  /**
    * For recursive structures.
    */
  def asLazy[A](lazyTypedJsonFormat: ⇒ TypedJsonFormat[A]) = new AsLazy[A] {
    lazy val delegate = lazyTypedJsonFormat
  }

  sealed trait AsLazy[A] extends RootJsonFormat[A] {
    def delegate: TypedJsonFormat[A]
    final def canSerialize(a: A) = delegate canSerialize a
    final def write(x: A) = delegate.write(x)
    final def read(value: JsValue) = delegate.read(value)
  }

  private[TypedJsonFormat] case class WriteEntry(jsonWriter: RootJsonWriter[Any], typeField: (String, JsString))
}
