package com.sos.jobscheduler.base.circeutils.typed

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import io.circe.{Decoder, HCursor, Json, JsonObject, ObjectEncoder}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class TypedJsonCodec[A](
  val classToEncoder: Map[Class[_], ObjectEncoder[_ <: A]],
  val nameToDecoder: Map[String, Decoder[_ <: A]],
  val nameToClass: Map[String, Class[_ <: A]])
extends ObjectEncoder[A] with Decoder[A]
{
  private val _classToName: Map[Class[_ <: A], String] =
    nameToClass.map(o ⇒ o._2 → o._1).toMap

  /** Union. */
  def |[B](other: TypedJsonCodec[B]): TypedJsonCodec[Any] = {
    val sameClasses = classToEncoder.keySet & other.classToEncoder.keySet
    if (sameClasses.nonEmpty) throw new IllegalArgumentException(s"Union of TypedJsonCodec has non-unique classes: $sameClasses")
    val sameClassNames = nameToClass.keySet & other.nameToClass.keySet
    if (sameClassNames.nonEmpty) throw new IllegalArgumentException(s"Union of TypedJsonCodec has non-unique decoder names: $sameClassNames")
    val sameDecoderNames = nameToDecoder.keySet & other.nameToDecoder.keySet
    if (sameDecoderNames.nonEmpty) throw new IllegalArgumentException(s"Union of TypedJsonCodec has non-unique class names: $sameDecoderNames")

    new TypedJsonCodec[Any](
      classToEncoder ++ other.classToEncoder,
      nameToDecoder ++ other.nameToDecoder,
      nameToClass ++ other.nameToClass)
  }

  def apply(c: HCursor) = decode(c)

  def encodeObject(a: A): JsonObject =
    classToEncoder(a.getClass).asInstanceOf[ObjectEncoder[A]].encodeObject(a)

  def decode(c: HCursor): Decoder.Result[A] =
    c.get[String](TypeFieldName) flatMap (o ⇒ nameToDecoder(o).apply(c))

  def canDeserialize(json: Json): Boolean =
    json.asObject match {
      case Some(o) ⇒ o.toMap.get(TypedJsonCodec.TypeFieldName) flatMap (_.asString) exists nameToDecoder.contains
      case _ ⇒ false
    }

  def typeName[A1 <: A](a: A1): String =
    _classToName(a.getClass)

  def classToName(getClass: Class[_ <: A]): String =
    _classToName(getClass)

  def classes[A1 <: A : ClassTag]: Set[Class[_ <: A1]] =
    classToEncoder.keySet collect {
      case c if implicitClass[A1] isAssignableFrom c ⇒ c.asInstanceOf[Class[A1]]
    }
}

object TypedJsonCodec {
  val TypeFieldName = "TYPE"

  def typeName[A: ClassTag]: String =
    typeName(implicitClass[A])

  def typeName(cls: Class[_]): String =
    cls.simpleScalaName

  def apply[A: ClassTag](subtypes: Subtype[_ <: A]*): TypedJsonCodec[A] = {
    val cls = implicitClass[A]
    new TypedJsonCodec[A](
      subtypes.flatMap(_.classToEncoder).uniqueToMap withDefault (o ⇒ throw new UnknownClassForJsonException(o, cls)),
      subtypes.flatMap(_.nameToDecoder).uniqueToMap withDefault (o ⇒ throw new UnknownJsonTypeException(o, cls)),
      subtypes.flatMap(_.nameToClass).uniqueToMap withDefault (o ⇒ throw new UnknownJsonTypeException(o, cls)))
  }

  final class UnknownClassForJsonException(subclass: Class[_], superclass: Class[_])
    extends NoSuchElementException(s"Class ${subclass.getName} is not registered with TypedJsonCodec[${superclass.getName}]")

  final class UnknownJsonTypeException(typeName: String, superclass: Class[_])
    extends RuntimeException(s"""Unexpected JSON {"$TypeFieldName": "$typeName"} for class '${superclass.simpleName}'""")
}
