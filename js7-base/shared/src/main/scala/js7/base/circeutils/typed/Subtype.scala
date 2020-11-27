package js7.base.circeutils.typed

import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils.singletonCodec
import js7.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, typeName}
import js7.base.utils.ScalaUtils.implicitClass
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
    fromClassName[A](implicitClass[A], Nil, implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]])

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
     fromClassName(implicitClass[A], Nil, encoder, decoder)

  /**
    * Use explicitly given Encoder.AsObject and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A: ClassTag](encoder: Encoder.AsObject[A], decoder: Decoder[A], subclasses: Iterable[Class[_/*crashes scalac 2.13.3: <: A*/]])
  : Subtype[A] =
    fromClassName[A](implicitClass[A], subclasses, encoder, decoder)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def apply[A: ClassTag](singleton: A) = {
    val codec = singletonCodec(singleton)
    fromClassName[A](implicitClass[A], Nil, codec, codec)
  }

  /**
    * Use implicit Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype.named[A]("name")
    */
  def named[A: ClassTag: Encoder.AsObject: Decoder](typeName: String) =
    of(implicitClass[A], Nil, typeName, implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]])

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec).
    * <p>
    * Usage: Subtype.named(codec, "name")
    */
  def named[A: ClassTag](codec: CirceObjectCodec[A], typeName: String): Subtype[A] =
    of(implicitClass[A], Nil, typeName, codec, codec)

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec).
    * <p>
    * Usage: Subtype.named(singleton, "name")
    */
  def named[A: ClassTag](singleton: A, typeName: String): Subtype[A] = {
    val codec = singletonCodec(singleton)
    of(implicitClass[A], Nil, typeName, codec, codec)
  }

  private def fromClassName[A](cls: Class[_], subclasses: Iterable[Class[_/* <: A*/]] = Nil, encoder: Encoder.AsObject[A], decoder: Decoder[A]) =
    of(cls, subclasses, typeName(cls), encoder, decoder)

  private def of[A](cls: Class[_], subclasses: Iterable[Class[_/* <: A*/]], typeName: String, encoder: Encoder.AsObject[A], decoder: Decoder[A]) = {
    import scala.language.existentials
    val typeField = TypeFieldName -> Json.fromString(typeName)
    val myNameToDecoder = Map(typeName -> decoder)
    val myNameClass = typeName -> cls.asInstanceOf[Class[_ <: A]]
    val myNameToClass = Map(myNameClass)
    new Subtype[A](
      classToEncoder = encoder match {
        case encoder: TypedJsonCodec[A] => encoder.classToEncoder
        case _ => (Iterable(cls) ++ subclasses).map(_ -> encoder.mapJsonObject(typeField +: _)).toMap
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
