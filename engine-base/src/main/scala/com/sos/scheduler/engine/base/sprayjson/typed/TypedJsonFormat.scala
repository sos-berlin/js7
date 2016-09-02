package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag
import spray.json._

/**
 * @author Joacim Zschimmer
 */
trait TypedJsonFormat[A] extends RootJsonFormat[A] with HasOwnTypeField[A] {

  def asJsObjectJsonFormat: TypedJsonFormat[A] = this

  def canSerialize(a: A): Boolean

  final lazy val classes: Set[Class[_ <: A]] = classToJsonWriter.keySet map { _.asInstanceOf[Class[_ <: A]] }
}

object TypedJsonFormat {
  val DefaultTypeFieldName = "TYPE"
  private val DefaultShortenTypeOnlyValue = false

  /**
    * A RootJsonType for polymorphic types.
    * <p><b>On compile error using TypedJsonFormat(Subtype ...):</b> check if every Subtype really denotes a subtype of A.
    */
  def apply[A: ClassTag](subtype: Subtype[_], subtypes: Subtype[_]*): TypedJsonFormat[A] =
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
    (subtypes: Subtype[_]*)
  : TypedJsonFormat[A] =
    _apply[A](typeFieldName = typeField, shortenTypeOnlyValue = shortenTypeOnlyValue, subtypes = subtypes)

  private def _apply[A: ClassTag](
    typeFieldName: String = DefaultTypeFieldName,
    shortenTypeOnlyValue: Boolean = DefaultShortenTypeOnlyValue,
    subtypes: Seq[Subtype[_]]): TypedJsonFormat[A]
  = {
    val superclassName = implicitClass[A].getSimpleName   // This name is only for users of typeToClass, like "Event"
    new WithSubtypeRegister[A](
      implicitClass[A],
      typeToClass = Map(superclassName → implicitClass[A]) ++
        (subtypes flatMap { _.nameToClass mapValues { _.asInstanceOf[Class[_ <: A]] }}).toMap,
      classToJsonWriter = (subtypes flatMap { _.toClassToJsonWriter(typeFieldName) }).toMap,
      typeToJsonReader = (subtypes flatMap { _.toTypeToReader(typeFieldName) }).toMap,
      typeFieldName = typeFieldName,
      shortenTypeOnlyValue = shortenTypeOnlyValue)
  }

  /**
    * For recursive structures.
    */
  def asLazy[A](lazyTypedJsonFormat: ⇒ TypedJsonFormat[A]): AsLazy[A] =
    new AsLazy[A] {
      lazy val delegate = lazyTypedJsonFormat
    }

  sealed trait AsLazy[A] extends RootJsonFormat[A] with CanSerialize[A] {
    def delegate: TypedJsonFormat[A]
    final def canSerialize(a: A) = delegate canSerialize a
    final def typeToClass = delegate.typeToClass
    final def write(x: A) = delegate.write(x)
    final def read(value: JsValue) = delegate.read(value)
  }
}
