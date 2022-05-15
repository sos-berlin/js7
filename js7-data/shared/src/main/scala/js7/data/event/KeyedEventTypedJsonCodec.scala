package js7.data.event

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import js7.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, UnknownClassForJsonException, typeName, unknownJsonTypeFailure}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Collections._
import js7.base.utils.Collections.implicits.RichPairTraversable
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEventTypedJsonCodec.KeyedSubtype
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTypedJsonCodec[E <: Event: ClassTag](
  private val superclassName: String,
  private val printName: String,
  private val subtypes: Seq[KeyedSubtype[_ <: E]])
extends Encoder.AsObject[KeyedEvent[E]]
with Decoder[KeyedEvent[E]]
{
  private val classToEncoder: Map[Class[_], Encoder.AsObject[KeyedEvent[_ <: E]]] =
    subtypes
      .flatMap(_.classToEncoder.view
        .mapValues(_.asInstanceOf[Encoder.AsObject[KeyedEvent[E]]]))
      .uniqueToMap
      .withDefault(o => throw new UnknownClassForJsonException(o.shortClassName, printName))

  private val nameToDecoder: Map[String, Decoder.Result[Decoder[KeyedEvent[_ <: E]]]] =
    subtypes
      .flatMap(_.nameToDecoder.view
        .mapValues(decoder => Right(decoder.asInstanceOf[Decoder[KeyedEvent[E]]])))
      .uniqueToMap
      .withDefault(typeName => Left(unknownJsonTypeFailure(typeName, printName, Nil)))

  private val nameToClass: Map[String, Class[_ <: E]] =
    subtypes.flatMap(_.nameToClass).uniqueToMap

  private val _classToName: Map[Class[_ <: E], String] =
    nameToClass.view.map(o => o._2 -> o._1).toMap

  private val classToNameJson: Map[Class[_ <: E], Json/*String*/] =
    _classToName.view.mapValues(Json.fromString).toMap

  /** Union. */
  def |[B <: Event](other: KeyedEventTypedJsonCodec[B]): KeyedEventTypedJsonCodec[Event] = {
    val sameClasses = classToEncoder.keySet & other.classToEncoder.keySet
    if (sameClasses.nonEmpty) throw new IllegalArgumentException(
      s"Union of KeyedEventTypedJsonCodec has non-unique classes: $sameClasses")

    val sameClassNames = nameToClass.keySet & other.nameToClass.keySet
    if (sameClassNames.nonEmpty) throw new IllegalArgumentException(
      s"Union of KeyedEventTypedJsonCodec has non-unique decoder names: $sameClassNames")

    val sameDecoderNames = nameToDecoder.keySet & other.nameToDecoder.keySet
    if (sameDecoderNames.nonEmpty) throw new IllegalArgumentException(
      s"Union of KeyedEventTypedJsonCodec has non-unique class names: $sameDecoderNames")

    new KeyedEventTypedJsonCodec[Event](
      superclassName = "Event",
      printName = s"$printName|${other.printName}",
      (subtypes ++ other.subtypes))
  }

  def encodeObject(keyedEvent: KeyedEvent[E]) =
    keyedEvent.asJsonObject(encoder = classToEncoder(keyedEvent.event.getClass))

  def apply(c: HCursor): Decoder.Result[KeyedEvent[E]] =
    for {
      typeName <- c.get[String](TypeFieldName)
      decoder <- nameToDecoder(typeName)
      keyedEvent <- decoder.apply(c)
    } yield keyedEvent

  implicit def keyedEventJsonCodec[EE <: E]: KeyedEventTypedJsonCodec[EE] =
    this.asInstanceOf[KeyedEventTypedJsonCodec[EE]]

  def canDeserialize(json: Json): Boolean =
    json.asObject match {
      case Some(o) => o.toMap.get(TypeFieldName).flatMap(_.asString) exists nameToDecoder.contains
      case _ => false
    }

  def isOfType[E1 <: E: ClassTag](json: Json): Boolean =
    json.asObject.flatMap(_(TypeFieldName)) contains classToNameJson(implicitClass[E1])

  def typenameToClassOption(name: String): Option[Class[_ <: E]] =
    if (name == this.superclassName)
      Some(implicitClass[E])
    else
      nameToClass.get(name)

  override def toString = printName
}

object KeyedEventTypedJsonCodec
{
  def apply[E <: Event: ClassTag](subtypes: KeyedSubtype[_ <: E]*)
    (implicit enclosing: sourcecode.Enclosing)
  : KeyedEventTypedJsonCodec[E] =
    named[E](
      enclosing.value + ": KeyedEventTypedJsonCodec[" + implicitClass[E].shortClassName + "]",
      subtypes: _*)

  def named[E <: Event: ClassTag](name: String, subtypes: KeyedSubtype[_ <: E]*)
  : KeyedEventTypedJsonCodec[E] =
    new KeyedEventTypedJsonCodec[E](implicitClass[E].simpleScalaName, printName = name, subtypes)

  final class KeyedSubtype[E <: Event](
    val classToEncoder: Map[Class[_], Encoder.AsObject[KeyedEvent[_ <: E]]],
    val nameToDecoder: Map[String, Decoder[KeyedEvent[_ <: E]]],
    val nameToClass: Map[String, Class[_ <: E]])(
    implicit private val classTag: ClassTag[E],
    implicit val encoder: Encoder.AsObject[KeyedEvent[E]],
    implicit val decoder: Decoder[KeyedEvent[E]],
    implicit val keyEncoder: Encoder[E#Key],
    implicit val keyDecoder: Decoder[E#Key],
    implicit val eventCodec: TypedJsonCodec[E])
  {
    type Event = E
    val eventClass: Class[E] = implicitClass[E]
    val typeName: String = eventClass.simpleScalaName
  }

  object KeyedSubtype {
    def apply[E <: Event: ClassTag](
      implicit ke: Encoder[E#Key], kd: Decoder[E#Key], codec: TypedJsonCodec[E])
    : KeyedSubtype[E] =
      make[E](typeName[E])

    def apply[E <: Event: ClassTag](singleton: E)
      (implicit ke: Encoder[E#Key], kd: Decoder[E#Key])
    : KeyedSubtype[E] = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype(singleton))
      make[E](typeName[E])
    }

    def singleEvent[E <: Event: ClassTag](
      implicit ke: Encoder[E#Key], kd: Decoder[E#Key],
      eventEncoder: Encoder.AsObject[E], eventDecoder: Decoder[E])
    : KeyedSubtype[E] = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype[E])
      make[E](typeName[E])
    }

    def singleEvent[E <: Event: ClassTag](singleton: E)
      (implicit ke: Encoder[E#Key], kd: Decoder[E#Key])
    : KeyedSubtype[E] = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype(singleton))
      make[E](typeName[E])
    }

    private def make[E <: Event: ClassTag](name: String)
      (implicit ke: Encoder[E#Key], kd: Decoder[E#Key], codec: TypedJsonCodec[E])
    : KeyedSubtype[E] =
      new KeyedSubtype[E](
        classToEncoder = codec.classToEncoder
          .mapValuesStrict(_ => KeyedEvent.jsonEncoder[E]),
        nameToDecoder = codec.nameToDecoder
          .mapValuesStrict(_ => KeyedEvent.jsonDecoder[E]),
        nameToClass = codec.nameToClass + (name -> implicitClass[E]))
  }
}
