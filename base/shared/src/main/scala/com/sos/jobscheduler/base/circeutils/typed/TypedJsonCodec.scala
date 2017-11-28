package com.sos.jobscheduler.base.circeutils.typed

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import io.circe.{Decoder, Encoder, HCursor, Json}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class TypedJsonCodec[A](
  val classToEncoder: Map[Class[_], Encoder[_ <: A]],
  val nameToDecoder: Map[String, Decoder[_ <: A]],
  val nameToClass: Map[String, Class[_ <: A]])
extends Encoder[A] with Decoder[A] {

  def apply(a: A) = encode(a)

  def apply(c: HCursor) = decode(c)

  def encode(a: A): Json =
    classToEncoder(a.getClass).asInstanceOf[Encoder[A]].apply(a)

  def decode(c: HCursor): Decoder.Result[A] =
    c.get[String](TypeFieldName) flatMap (o ⇒ nameToDecoder(o).apply(c))

  def canDeserialize(json: Json): Boolean =
    json.asObject match {
      case Some(o) ⇒ o.toMap.get(TypedJsonCodec.TypeFieldName) flatMap (_.asString) exists nameToDecoder.contains
      case _ ⇒ false
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
    extends NoSuchElementException(s"Class ${subclass.getName} is not registered with JsonTypeCodec[${superclass.getName}]")

  final class UnknownJsonTypeException(typeName: String, superclass: Class[_])
    extends RuntimeException(s"""Unexpected JSON { "$TypeFieldName": "$typeName" } for class '${superclass.simpleName}'""")
}
