package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, UnknownClassForJsonException, UnknownJsonTypeException, typeName, unknownJsonTypeFailure}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections._
import com.sos.jobscheduler.base.utils.Collections.implicits.RichPairTraversable
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTypedJsonCodec[E <: Event: ClassTag](
  val name: String,
  val printName: String,
  val classToEncoder: Map[Class[_], Encoder.AsObject[KeyedEvent[_ <: E]]],
  val nameToDecoder: Map[String, Decoder.Result[Decoder[KeyedEvent[_ <: E]]]],
  val nameToClass: Map[String, Class[_ <: E]])
extends Encoder.AsObject[KeyedEvent[E]]
with Decoder[KeyedEvent[E]]
{
  private val _classToName: Map[Class[_ <: E], String] =
    nameToClass.map(o => o._2 -> o._1).toMap

  private val classToNameJson: Map[Class[_ <: E], Json/*String*/] =
    _classToName.view.mapValues(Json.fromString).toMap

  /** Union. */
  def |[B <: Event](other: KeyedEventTypedJsonCodec[B]): KeyedEventTypedJsonCodec[Event] = {
    val sameClasses = classToEncoder.keySet & other.classToEncoder.keySet
    if (sameClasses.nonEmpty) throw new IllegalArgumentException(s"Union of KeyedEventTypedJsonCodec has non-unique classes: $sameClasses")
    val sameClassNames = nameToClass.keySet & other.nameToClass.keySet
    if (sameClassNames.nonEmpty) throw new IllegalArgumentException(s"Union of KeyedEventTypedJsonCodec has non-unique decoder names: $sameClassNames")
    val sameDecoderNames = nameToDecoder.keySet & other.nameToDecoder.keySet
    if (sameDecoderNames.nonEmpty) throw new IllegalArgumentException(s"Union of KeyedEventTypedJsonCodec has non-unique class names: $sameDecoderNames")

    new KeyedEventTypedJsonCodec[Event](
      name = "Event",
      printName = s"$printName|${other.printName}",
      (classToEncoder.asInstanceOf[Map[Class[_ <: Event], Encoder.AsObject[KeyedEvent[_ <: Event]]]] ++
        other.classToEncoder.asInstanceOf[Map[Class[_ <: Event], Encoder.AsObject[KeyedEvent[_ <: Event]]]]
      ).toMap,
      nameToDecoder.asInstanceOf[Map[String, Decoder.Result[Decoder[KeyedEvent[_ <: Event]]]]] ++
        other.nameToDecoder.asInstanceOf[Map[String, Decoder.Result[Decoder[KeyedEvent[_ <: Event]]]]],
      nameToClass.asInstanceOf[Map[String, Class[_ <: Event]]] ++
        other.nameToClass.asInstanceOf[Map[String, Class[_ <: Event]]])
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
      case Some(o) => o.toMap.get(TypeFieldName) flatMap (_.asString) exists nameToDecoder.contains
      case _ => false
    }

  def isOfType[E1 <: E: ClassTag](json: Json): Boolean =
    json.asObject.flatMap(_(TypeFieldName)) contains classToNameJson(implicitClass[E1])

  def typenameToClassOption(name: String): Option[Class[_ <: E]] =
    if (name == this.name)
      Some(implicitClass[E])
    else
      nameToClass.get(name)

  override def toString = s"KeyedEventTypedJsonCodec[$printName]"
}

object KeyedEventTypedJsonCodec
{
  def apply[E <: Event: ClassTag](subtypes: KeyedSubtype[_ <: E]*) = {
    val cls = implicitClass[E]
    new KeyedEventTypedJsonCodec[E](
      cls.simpleScalaName,
      cls.simpleScalaName,
      subtypes.flatMap(_.classToEncoder mapValuesStrict (_.asInstanceOf[Encoder.AsObject[KeyedEvent[E]]])).uniqueToMap withDefault (o => throw new UnknownClassForJsonException(o, cls)),
      subtypes.flatMap(_.nameToDecoder.mapValuesStrict (decoder =>
        Right(decoder.asInstanceOf[Decoder[KeyedEvent[E]]]))).uniqueToMap
          .withDefault(typeName => Left(unknownJsonTypeFailure(typeName, cls))),
      subtypes.flatMap(_.nameToClass).uniqueToMap withDefault (o => throw new UnknownJsonTypeException(o, cls)))
  }

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
    def apply[E <: Event: ClassTag](implicit ke: Encoder[E#Key], kd: Decoder[E#Key], codec: TypedJsonCodec[E]): KeyedSubtype[E] =
      of[E](typeName[E])

    def singleEvent[E <: Event: ClassTag](implicit ke: Encoder[E#Key], kd: Decoder[E#Key], eventEncoder: Encoder.AsObject[E], eventDecoder: Decoder[E]): KeyedSubtype[E] = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype[E])
      of[E](typeName[E])
    }

    def apply[E <: Event: ClassTag](singleton: E)(implicit ke: Encoder[E#Key], kd: Decoder[E#Key]) = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype(singleton))
      of[E](typeName[E])
    }

    def of[E <: Event: ClassTag](name: String)(implicit ke: Encoder[E#Key], kd: Decoder[E#Key], codec: TypedJsonCodec[E]): KeyedSubtype[E] = {
      new KeyedSubtype[E](
        classToEncoder = codec.classToEncoder.mapValuesStrict { _ =>
          KeyedEvent.jsonEncoder[E]
        },
        nameToDecoder = codec.nameToDecoder.mapValuesStrict { _ =>
          KeyedEvent.jsonDecoder[E]
        },
        nameToClass = codec.nameToClass + (name -> implicitClass[E])
      )
    }
  }
}
