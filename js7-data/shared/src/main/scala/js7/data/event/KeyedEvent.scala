package js7.data.event

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.KeyedEvent.NoKey
import scala.reflect.ClassTag

/**
  * A [[Event]] enriched with a `key` designating the respective object.
  *
  * @author Joacim Zschimmer
  */
final case class KeyedEvent[+E <: Event](key: E#Key, event: E)
{
  override def toString = s"$keyPrefix$event"

  def toShortString = s"$keyPrefix${TypedJsonCodec.typeName(event.getClass)}"

  private def keyPrefix = if (key == NoKey) "" else s"$key <-: "
}

object KeyedEvent {
  private[event] val KeyFieldName = "Key"

  type NoKey = NoKey.type
  case object NoKey {
    implicit val jsonEncoder: Encoder[NoKey] = _ => sys.error("NoKey Encoder")
    implicit val jsonDecoder: Decoder[NoKey] = _ => sys.error("NoKey Decoder")

    override def toString = "NoKey"
  }

  def apply[E <: Event](event: E)(key: event.Key) = new KeyedEvent(key, event)

  def apply[E <: Event { type Key = NoKey }](event: E) = new KeyedEvent[E](NoKey, event)

  def of[E <: Event { type Key = NoKey }](event: E) = new KeyedEvent[E](NoKey, event)

  implicit def jsonEncoder[E <: Event](implicit eventEncoder: Encoder.AsObject[E], keyEncoder: Encoder[E#Key]): Encoder.AsObject[KeyedEvent[E]] =
    keyedEvent => {
      val jsonObject = keyedEvent.event.asJsonObject
      keyedEvent.key match {
        case _: NoKey.type => jsonObject
        case key =>
          require(!jsonObject.contains(KeyFieldName), s"Serialized ${keyedEvent.getClass} must not contain a field '$KeyFieldName'")
          (KeyFieldName -> key.asJson) +: jsonObject
      }
    }

  implicit def jsonDecoder[E <: Event](implicit decoder: Decoder[E], keyDecoder: Decoder[E#Key]): Decoder[KeyedEvent[E]] =
    cursor => for {
      key <- cursor.getOrElse[E#Key](KeyFieldName)(NoKey.asInstanceOf[E#Key])
      event <- cursor.as[E]
    } yield KeyedEvent(key, event)

  def typedJsonCodec[E <: Event: ClassTag](subtypes: KeyedEventTypedJsonCodec.KeyedSubtype[? <: E]*) =
    KeyedEventTypedJsonCodec[E](subtypes: _*)
}
