package js7.base.circeutils.typed

import io.circe.{Codec, Decoder, Encoder, Json}
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils.singletonCodec
import js7.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, typeName}
import js7.base.utils.Collections.implicits.RichPairTraversable
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
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype(codec)
    */
  def apply[A](codec: Codec.AsObject[A])
    (implicit classTag: ClassTag[A])
  : Subtype[A] =
    apply(codec, codec)

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype(codec)
    */
  def apply[A](codec: Codec.AsObject[A], aliases: Seq[String])
    (implicit classTag: ClassTag[A])
  : Subtype[A] =
    apply(codec, codec, aliases = aliases)

  /**
    * Use explicitly given Encoder.AsObject and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A](encoder: Encoder.AsObject[A], decoder: Decoder[A])
    (implicit classTag: ClassTag[A])
  : Subtype[A] =
   fromClassName(implicitClass[A], Nil, encoder, decoder)

  /**
    * Use explicitly given Encoder.AsObject and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A](encoder: Encoder.AsObject[A], decoder: Decoder[A], aliases: Seq[String] = Nil)
    (implicit classTag: ClassTag[A])
  : Subtype[A] =
   fromClassName(implicitClass[A], Nil, encoder, decoder, aliases = aliases)

  /**
    * Use explicitly given Encoder.AsObject and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A: ClassTag](encoder: Encoder.AsObject[A], decoder: Decoder[A],
    subclasses: Iterable[Class[_<: A]])
  : Subtype[A] =
    fromClassName[A](implicitClass[A], subclasses, encoder, decoder)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def apply[A: ClassTag](singleton: A) =
    this.singleton(singleton)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def singleton[A: ClassTag](singleton: A, aliases: Seq[String] = Nil) = {
    val codec = singletonCodec(singleton)
    fromClassName[A](implicitClass[A], Nil, codec, codec, aliases = aliases)
  }

  def withAliases[A: ClassTag](codec: Codec.AsObject[A], aliases: Seq[String]) =
    of(implicitClass[A], Nil, typeName(implicitClass[A]),
      codec, codec,
      aliases = aliases)

  /**
    * Use implicit Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype.named[A](aliases = Seq("OldName"))
    */
  def named[A: ClassTag: Encoder.AsObject: Decoder](aliases: Seq[String]) =
    of(implicitClass[A], Nil, typeName(implicitClass[A]),
      implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]],
      aliases = aliases)

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
    * Usage: Subtype.named(codec, "name")
    */
  def named[A: ClassTag](codec: Codec.AsObject[A], typeName: String): Subtype[A] =
    of(implicitClass[A], Nil, typeName, codec, codec)

  def named[A: ClassTag: Encoder.AsObject: Decoder](
    subclasses: Iterable[Class[_ <: A]],
    aliases: Seq[String])
  : Subtype[A] =
    fromClassName[A](implicitClass[A], subclasses, implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]],
      aliases = aliases)

  def named[A: ClassTag](
    encoder: Encoder.AsObject[A],
    decoder: Decoder[A],
    subclasses: Iterable[Class[_ <: A]],
    aliases: Seq[String])
  : Subtype[A] =
    fromClassName[A](implicitClass[A], subclasses, encoder, decoder, aliases)

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec).
    * <p>
    * Usage: Subtype.named(singleton, "name")
    */
  def named[A: ClassTag](singleton: A, typeName: String): Subtype[A] = {
    val codec = singletonCodec(singleton)
    of(implicitClass[A], Nil, typeName, codec, codec)
  }

  private def fromClassName[A](
    cls: Class[_],
    subclasses: Iterable[Class[_/* <: A*/]],
    encoder: Encoder.AsObject[A],
    decoder: Decoder[A],
    aliases: Seq[String] = Nil)
  =
    of(cls, subclasses, typeName(cls), encoder, decoder, aliases)

  private def of[A](
    cls: Class[_],
    subclasses: Iterable[Class[_/* <: A*/]],
    typeName: String,
    encoder: Encoder.AsObject[A],
    decoder: Decoder[A],
    aliases: Seq[String] = Nil)
  = {
    val typeField = TypeFieldName -> Json.fromString(typeName)
    val names = typeName +: aliases
    val myNamesToDecoder = names.map(_ -> decoder).uniqueToMap
    val myNamesToClass = names.map(_ -> cls.asInstanceOf[Class[_ <: A]]).uniqueToMap
    new Subtype[A](
      classToEncoder = encoder match {
        case encoder: TypedJsonCodec[A] => encoder.classToEncoder
        case _ => (Iterable(cls) ++ subclasses).map(_ -> encoder.mapJsonObject(typeField +: _)).toMap
      },
      nameToDecoder = decoder match {
        case decoder: TypedJsonCodec[A] => decoder.nameToDecoder /*++ myNamesToDecoder???*/
        case _ => myNamesToDecoder
      },
      nameToClass = decoder match {
        case decoder: TypedJsonCodec[A] => decoder.nameToClass ++ myNamesToClass
        case _ => myNamesToClass
      })
  }
}
