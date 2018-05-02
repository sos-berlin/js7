package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.{TypeFieldName, UnknownClassForJsonException, UnknownJsonTypeException, typeName}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichPairTraversable
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json, ObjectEncoder}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class KeyedEventTypedJsonCodec[E <: Event: ClassTag](
  val name: String,
  val classToEncoder: Map[Class[_], ObjectEncoder[KeyedEvent[_ <: E]]],
  val nameToDecoder: Map[String, Decoder[KeyedEvent[_ <: E]]],
  val nameToClass: Map[String, Class[_ <: E]])
extends ObjectEncoder[KeyedEvent[E]]
with Decoder[KeyedEvent[E]] {

  def encodeObject(keyedEvent: KeyedEvent[E]) =
    keyedEvent.asJsonObject(encoder = classToEncoder(keyedEvent.event.getClass))

  def apply(c: HCursor): Decoder.Result[KeyedEvent[E]] =
    for {
      typeName ← c.get[String](TypeFieldName)
      keyedEvent ← nameToDecoder(typeName)(c)
    } yield keyedEvent

  implicit def keyedEventJsonCodec[EE <: E]: KeyedEventTypedJsonCodec[EE] =
    this.asInstanceOf[KeyedEventTypedJsonCodec[EE]]

  def canDeserialize(json: Json): Boolean =
    json.asObject match {
      case Some(o) ⇒ o.toMap.get(TypeFieldName) flatMap (_.asString) exists nameToDecoder.contains
      case _ ⇒ false
    }

  def typenameToClassOption(name: String): Option[Class[_ <: E]] =
    if (name == this.name)
      Some(implicitClass[E])
    else
      nameToClass.get(name)
}

object KeyedEventTypedJsonCodec {

  def apply[E <: Event: ClassTag](subtypes: KeyedSubtype[_ <: E]*) = {
    val cls = implicitClass[E]
    new KeyedEventTypedJsonCodec[E](
      cls.simpleScalaName,
      subtypes.flatMap(_.classToEncoder mapValues (_.asInstanceOf[ObjectEncoder[KeyedEvent[E]]])).uniqueToMap withDefault (o ⇒ throw new UnknownClassForJsonException(o, cls)),
      subtypes.flatMap(_.nameToDecoder.mapValues (_.asInstanceOf[Decoder[KeyedEvent[E]]])).uniqueToMap withDefault (o ⇒ throw new UnknownJsonTypeException(o, cls)),
      subtypes.flatMap(_.nameToClass).uniqueToMap withDefault (o ⇒ throw new UnknownJsonTypeException(o, cls)))
  }

  final class KeyedSubtype[E <: Event](
    val classToEncoder: Map[Class[_], ObjectEncoder[KeyedEvent[_ <: E]]],
    val nameToDecoder: Map[String, Decoder[KeyedEvent[_ <: E]]],
    val nameToClass: Map[String, Class[_ <: E]])(
    implicit private val classTag: ClassTag[E],
    implicit val encoder: ObjectEncoder[KeyedEvent[E]],
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

    def singleEvent[E <: Event: ClassTag](implicit ke: Encoder[E#Key], kd: Decoder[E#Key], eventEncoder: ObjectEncoder[E], eventDecoder: Decoder[E]): KeyedSubtype[E] = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype[E])
      of[E](typeName[E])
    }

    def apply[E <: Event: ClassTag](singleton: E)(implicit ke: Encoder[E#Key], kd: Decoder[E#Key]) = {
      implicit val typedJsonCodec = TypedJsonCodec[E](Subtype(singleton))
      of[E](typeName[E])
    }

    def of[E <: Event: ClassTag](name: String)(implicit ke: Encoder[E#Key], kd: Decoder[E#Key], codec: TypedJsonCodec[E]): KeyedSubtype[E] = {
      new KeyedSubtype[E](
        classToEncoder = codec.classToEncoder.mapValues { encoder ⇒
          implicit val x = encoder.asInstanceOf[ObjectEncoder[E]]
          KeyedEvent.jsonEncoder[E]
        },
        nameToDecoder = codec.nameToDecoder.mapValues { decoder ⇒
          implicit val x = decoder.asInstanceOf[Decoder[E]]
          KeyedEvent.jsonDecoder[E]
        },
        nameToClass = codec.nameToClass + (name → implicitClass[E])
      )
    }
  }
}
