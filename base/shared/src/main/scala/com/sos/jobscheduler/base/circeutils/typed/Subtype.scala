package com.sos.jobscheduler.base.circeutils.typed

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.singletonCodec
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, typeName}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import io.circe.{Decoder, Encoder, Json}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class Subtype[A](
  val classToEncoder: Map[Class[_], Encoder.AsObject[_ <: A]],
  val nameToDecoder: Map[String, Decoder[_ <: A]],
  val nameToClass: Map[String, Class[_ <: A]])

object Subtype {
  /**
    * Use implicit Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype[A]
    */
  def apply[A: ClassTag: Encoder.AsObject: Decoder] =
    fromClassName(implicitClass[A], implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]])

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype(codec)
    */
  def apply[A](codec: CirceObjectCodec[A])(implicit classTag: ClassTag[A]): Subtype[A] =
    apply(codec, codec)

  /**
    * Use explicitly given Encoder.AsObject and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A](encoder: Encoder.AsObject[A], decoder: Decoder[A])(implicit classTag: ClassTag[A]): Subtype[A] =
    fromClassName(implicitClass[A], encoder, decoder)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def apply[A: ClassTag](singleton: A) = {
    val codec = singletonCodec(singleton)
    fromClassName(implicitClass[A], codec, codec)
  }

  /**
    * Use implicit Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype.named[A]("name")
    */
  def named[A: ClassTag: Encoder.AsObject: Decoder](typeName: String) =
    of(implicitClass[A], typeName, implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]])

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec).
    * <p>
    * Usage: Subtype.named(codec, "name")
    */
  def named[A: ClassTag](codec: CirceObjectCodec[A], typeName: String): Subtype[A] =
    of(implicitClass[A], typeName, codec, codec)

  private def fromClassName[A](cls: Class[_], encoder: Encoder.AsObject[A], decoder: Decoder[A]) =
    of(cls, typeName(cls), encoder, decoder)

  private def of[A](cls: Class[_], typeName: String, encoder: Encoder.AsObject[A], decoder: Decoder[A]) = {
    import scala.language.existentials
    val typeField = TypeFieldName -> Json.fromString(typeName)
    val myNameToDecoder = Map(typeName -> decoder)
    val myNameClass = typeName -> cls.asInstanceOf[Class[_ <: A]]
    val myNameToClass = Map(myNameClass)
    new Subtype[A](
      classToEncoder = encoder match {
        case encoder: TypedJsonCodec[A] => encoder.classToEncoder
        case _ => Map(cls -> encoder.mapJsonObject(o => typeField +: o))
      },
      nameToDecoder = decoder match {
        case decoder: TypedJsonCodec[A] => decoder.nameToDecoder
        case _ => myNameToDecoder
      },
      nameToClass = {
        decoder match {
          case decoder: TypedJsonCodec[A] => decoder.nameToClass + myNameClass
          case _ => myNameToClass
        }
      })
  }
}
