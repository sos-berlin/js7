package js7.base.circeutils.typed

import io.circe.{Codec, Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, typeName}
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichPairTraversable
import js7.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class Subtype[A](
  val classToEncoder: Map[Class[?], Encoder.AsObject[A]],
  val nameToDecoder: Map[String, Decoder[A]],
  val nameToClass: Map[String, Class[A]])


object Subtype:
  /**
    * Use implicit Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype[A]
    */
  def apply[A: ClassTag: Encoder.AsObject: Decoder]: Subtype[A] =
    make[A](implicitClass[A] :: Nil, Some(implicitClass[A]),
      typeName[A], implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]])

  def apply[A: ClassTag: Encoder.AsObject: Decoder](aliases: Seq[String]): Subtype[A] =
    apply(Nil, aliases)

  def apply[A: ClassTag: Encoder.AsObject: Decoder](
    subclasses: Iterable[Class[? <: A]],
    aliases: Seq[String] = Nil)
  : Subtype[A] =
    make[A](Seq(implicitClass[A]) ++ subclasses, Some(implicitClass[A]),
      typeName[A], implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]], aliases)

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype(codec)
    */
  def apply[A: ClassTag](codec: Codec.AsObject[A]): Subtype[A] =
    make[A](implicitClass[A] :: Nil, Some(implicitClass[A]), typeName[A], codec, codec, Nil)

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype(codec)
    */
  def apply[A: ClassTag](codec: Codec.AsObject[A], aliases: Seq[String]): Subtype[A] =
    make[A](implicitClass[A] :: Nil, Some(implicitClass[A]),
      typeName[A], codec, codec, aliases = aliases)

  /**
    * Use explicitly given Encoder.AsObject and Decoder; Simple class name is type name.
    * <p>
    * Usage: Subtype(encoder, decoder)
    */
  def apply[A: ClassTag](encoder: Encoder.AsObject[A], decoder: Decoder[A]): Subtype[A] =
   make[A](implicitClass[A] :: Nil, Some(implicitClass[A]), typeName[A], encoder, decoder)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def apply[A: ClassTag](singleton: A): Subtype[A] =
    this.singleton(singleton)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def singleton[A: ClassTag](singleton: A, aliases: Seq[String] = Nil): Subtype[A] =
    renamedSingleton(singleton, typeName[A], aliases)

  /**
    * Singleton is serialized as empty JSON object; Simple class name (without trailing $) is type name.
    * <p>
    * Usage: Subtype(A)
    */
  def renamedSingleton[A: ClassTag](singleton: A, name: String, aliases: Seq[String] = Nil)
  : Subtype[A] =
    val codec = singletonCodec(singleton)
    make[A](implicitClass[A] :: Nil, Some(implicitClass[A]), name, codec, codec, aliases = aliases)

  /**
    * Use implicit Encoder.AsObject and Decoder (CirceCodec); Simple class name is type name.
    * <p>
    * Usage: Subtype.named[A]("name")
    */
  def named1[A: ClassTag: Encoder.AsObject: Decoder](
    typeName: String,
    subclasses: Iterable[Class[? <: A]] = Nil,
    aliases: Seq[String] = Nil)
  : Subtype[A] =
    make(
      if subclasses.isEmpty then Seq(implicitClass[A]) else subclasses,
      Some(implicitClass[A]),
      typeName, implicitly[Encoder.AsObject[A]], implicitly[Decoder[A]], aliases)

  /**
    * Use explicitly given Encoder.AsObject and Decoder (CirceCodec).
    * <p>
    * Usage: Subtype.named(codec, "name")
    */
  def named[A: ClassTag](codec: Codec.AsObject[A], typeName: String): Subtype[A] =
    make[A](implicitClass[A] :: Nil, Some(implicitClass[A]), typeName, codec, codec)

  def decodeCompatible[Old: ClassTag, New: ClassTag](decoder: Decoder[Old])(toNew: Old => Checked[New])
  : Subtype[New] =
    make(Nil, Some(implicitClass[New]), typeName[Old],
      noEncoder,
      c => decoder(c)
        .flatMap(old => toNew(old).toDecoderResult(c.history)))

  private def noEncoder[A]: Encoder.AsObject[A] =
    _ => throw new UnsupportedOperationException(s"No Encoder")

  private def make[A](
    encodingClasses: Iterable[Class[?]],
    decodingSuperclass: Option[Class[?]],
    typeName: String,
    encoder: Encoder.AsObject[A],
    decoder: Decoder[A],
    aliases: Seq[String] = Nil)
  =
    val typeField = TypeFieldName -> Json.fromString(typeName)
    val names = typeName +: aliases
    val myNamesToDecoder = names.map(_ -> decoder).uniqueToMap
    val myNamesToClass = decodingSuperclass.view
      .flatMap(cls => names.map(_ -> cls.asInstanceOf[Class[A]]))
      .uniqueToMap
    new Subtype[A](
      classToEncoder = encoder match {
        case encoder: TypedJsonCodec[A] => encoder.classToEncoder
        case _ => encodingClasses.map(_ -> encoder.mapJsonObject(typeField +: _)).toMap
      },
      nameToDecoder = decoder match {
        case decoder: TypedJsonCodec[A] => decoder.nameToDecoder /*++ myNamesToDecoder???*/
        case _ => myNamesToDecoder
      },
      nameToClass = decoder match {
        case decoder: TypedJsonCodec[A] => decoder.nameToClass ++ myNamesToClass
        case _ => myNamesToClass
      })
