package com.sos.jobscheduler.base.circeutils.typed

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichJson, objectCodec}
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, typeName}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import io.circe.{Decoder, Encoder, Json}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class Subtype[A](
  val classToEncoder: Map[Class[_], Encoder[_ <: A]],
  val nameToDecoder: Map[String, Decoder[_ <: A]],
  val nameToClass: Map[String, Class[_ <: A]])

object Subtype {
  /**
    * Use implicit Encoder and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype[A]
    */
  def apply[A: ClassTag: Encoder: Decoder] =
    fromClassName(implicitClass[A], implicitly[Encoder[A]], implicitly[Decoder[A]])

  /**
    * Use explicitly given Encoder and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype(codec)
    */
  def apply[A](codec: CirceCodec[A])(implicit classTag: ClassTag[A]): Subtype[A] =
    apply(codec, codec)

  /**
    * Use explicitly given Encoder and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A](encoder: Encoder[A], decoder: Decoder[A])(implicit classTag: ClassTag[A]): Subtype[A] =
    fromClassName(implicitClass[A], encoder, decoder)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def apply[A: ClassTag](singleton: A) = {
    val codec = objectCodec(singleton)
    fromClassName(implicitClass[A], codec, codec)
  }

  /**
    * Use implicit Encoder and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype.named[A]("name")
    */
  def named[A: ClassTag: Encoder: Decoder](typeName: String) =
    of(implicitClass[A], typeName, implicitly[Encoder[A]], implicitly[Decoder[A]])

  /**
    * Use explicitly given Encoder and Decoder (CirceCodec).
    * <p>
    * Usage: Subtype.named(codec, "name")
    */
  def named[A: ClassTag](codec: CirceCodec[A], typeName: String): Subtype[A] =
    of(implicitClass[A], typeName, codec, codec)

  private def fromClassName[A](cls: Class[_], encoder: Encoder[A], decoder: Decoder[A]) =
    of(cls, typeName(cls), encoder, decoder)

  private def of[A](cls: Class[_], typeName: String, encoder: Encoder[A], decoder: Decoder[A]) =
    new Subtype[A](
      classToEncoder = encoder match {
        case encoder: TypedJsonCodec[A] ⇒ encoder.classToEncoder
        case _ ⇒
          val typ = TypeFieldName → Json.fromString(typeName)
          Map(cls → encoder.mapJson(o ⇒ Json.fromJsonObject(typ +: o.forceObject)))
      },
      nameToDecoder = decoder match {
        case decoder: TypedJsonCodec[A] ⇒ decoder.nameToDecoder
        case _ ⇒ Map(typeName → decoder)
      },
      nameToClass = {
        val a = typeName → cls.asInstanceOf[Class[_ <: A]]
        decoder match {
          case decoder: TypedJsonCodec[A] ⇒ decoder.nameToClass + a
          case _ ⇒ Map(a)
        }
      })
}
